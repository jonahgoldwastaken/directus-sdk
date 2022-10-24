/**
 * Fields handler
 */

import { ManyItems, OneItem, ItemInput, EmptyParamError, DefaultItem } from '../items';
import { ITransport } from '../transport';
import { FieldType, DefaultType, ID } from '../types';

export type FieldItem<T = DefaultType> = FieldType & T;

export class FieldsHandler<T = FieldItem> {
	transport: ITransport;
	constructor(transport: ITransport) {
		this.transport = transport;
	}

	async readOne(collection: string, id: ID): Promise<OneItem<T>> {
		if (`${collection}` === '') throw new EmptyParamError('collection');
		if (`${id}` === '') throw new EmptyParamError('id');
		const response = await this.transport.get(`/fields/${collection}/${id}`);
		return response.data as OneItem<T>;
	}

	async readMany(collection: string): Promise<ManyItems<T>> {
		if (`${collection}` === '') throw new EmptyParamError('collection');
		const response = await this.transport.get(`/fields/${collection}`);

		return {
			data: response.data as T[],
			meta: undefined,
		};
	}

	async readAll(): Promise<ManyItems<T>> {
		const response = await this.transport.get(`/fields`);
		return {
			data: response.data as T[],
			meta: undefined,
		};
	}

	async createOne(collection: string, item: ItemInput<T>): Promise<OneItem<T>> {
		if (`${collection}` === '') throw new EmptyParamError('collection');
		return (await this.transport.post<DefaultItem<T>>(`/fields/${collection}`, item)).data;
	}

	async updateOne(collection: string, field: string, item: ItemInput<T>): Promise<OneItem<T>> {
		if (`${collection}` === '') throw new EmptyParamError('collection');
		if (`${field}` === '') throw new EmptyParamError('field');
		return (await this.transport.patch<DefaultItem<T>>(`/fields/${collection}/${field}`, item)).data;
	}

	async deleteOne(collection: string, field: string): Promise<void> {
		if (`${collection}` === '') throw new EmptyParamError('collection');
		if (`${field}` === '') throw new EmptyParamError('field');
		await this.transport.delete(`/fields/${collection}/${field}`);
	}
}
