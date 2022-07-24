import { TransportRequestOptions } from './transport';
import { ID } from './types';

export type Field = string;

export type Item = Record<string, any>;

export type PartialItem<T> = {
	[P in keyof T]?: T[P] extends Record<string, any> ? PartialItem<T[P]> : T[P];
};

export type OneItem<T extends Item, Q extends QueryOne<T> = Record<string, any>, F = QueryFields<Q>> =
	| (F extends false ? PartialItem<T> : PickedPartialItem<T, F>)
	| null
	| undefined;

export type ManyItems<T extends Item, Q extends QueryMany<T> = Record<string, any>> = {
	data?: OneItem<T, Q>[] | null;
	meta?: ItemMetadata;
};

export type ItemMetadata = {
	total_count?: number;
	filter_count?: number;
};

export type Payload = Record<string, any>;

export enum Meta {
	TOTAL_COUNT = 'total_count',
	FILTER_COUNT = 'filter_count',
}

export type QueryFields<Q extends Record<string, any>> = Q extends Record<'fields', any>
	? Q['fields'] extends string
		? [Q['fields']]
		: Q['fields'] extends string[]
		? Q['fields']
		: false
	: false;

type DeepPathToObject<Path extends string, T extends Record<string, any>> = string extends Path
	? never
	: Path extends `${infer Key}.${infer Rest}`
	? Key extends keyof T
		? {
				[K in Key]?: TreeBranch<T[K], Rest>;
		  }
		: Key extends '*'
		? Rest extends `${infer NextVal}.${string}`
			? {
					[K in keyof T]: NextVal extends keyof T[K] ? TreeBranch<T[K], Rest> : never;
			  }
			: Rest extends '*'
			? {
					[K in keyof T]?: TreeBranch<T[K], Rest>;
			  }
			: {
					[K in keyof T]?: Rest extends keyof T[K] ? TreeBranch<T[K], Rest> : never;
			  }
		: never
	: Path extends keyof T
	? {
			[K in Path]?: TreeLeaf<T[K]>;
	  }
	: Path extends '*'
	? {
			[K in keyof T]?: TreeLeaf<T[K]>;
	  }
	: never;

type TreeBranch<T, Path extends string, NT = NonNullable<T>> = NT extends (infer U)[]
	? IsUnion<NonNullable<U>> extends true
		? DeepPathToObject<Path, Extract<NonNullable<U>, Record<string, any>>>
		: DeepPathToObject<Path, NonNullable<U>>
	: IsUnion<NT> extends true
	? DeepPathToObject<Path, Extract<NT, Record<string, any>>>
	: DeepPathToObject<Path, NT>;

type TreeLeaf<T, NT = NonNullable<T>> = NT extends (infer U)[]
	? IsUnion<NonNullable<U>> extends true
		? Extract<NonNullable<U>, Record<string, any>> extends Record<string, any>
			? Exclude<NonNullable<U>, Record<string, any>>
			: NonNullable<U>
		: undefined
	: IsUnion<NT> extends true
	? Extract<NT, Record<string, any>> extends Record<string, any>
		? Exclude<NT, Record<string, any>>
		: NT
	: NT extends Record<string, any>
	? undefined
	: NT;

type UnionToIntersectionFn<TUnion> = (TUnion extends TUnion ? (union: () => TUnion) => void : never) extends (
	intersection: infer Intersection
) => void
	? Intersection
	: never;

type LastUnion<TUnion> = UnionToIntersectionFn<TUnion> extends () => infer Last ? Last : never;

type UnionToTuple<TUnion, TResult extends Array<unknown> = []> = TUnion[] extends never[]
	? TResult
	: UnionToTuple<Exclude<TUnion, LastUnion<TUnion>>, [...TResult, LastUnion<TUnion>]>;

export type PickedPartialItem<T extends Item, Fields> = Fields extends string[]
	? UnionToTuple<Fields[number]> extends [infer First, ...infer Rest]
		? First extends string
			? Rest extends string[]
				? IntersectionToObject<
						Rest['length'] extends 0
							? DeepPathToObject<First, T>
							: DeepPathToObject<First, T> & PickedPartialItem<T, Rest>
				  >
				: never
			: never
		: never
	: never;

type IntersectionToObject<U> = U extends infer O ? { [K in keyof O]: O[K] } : never;

export type QueryOne<T> = {
	fields?: DotSeparated<T, 3> | DotSeparated<T, 3>[] | string | string[];
	search?: string;
	deep?: Deep<T>;
	export?: 'json' | 'csv' | 'xml';
	filter?: Filter<T>;
};

export type QueryMany<T> = QueryOne<T> & {
	sort?: Sort<T>;
	limit?: number;
	offset?: number;
	page?: number;
	meta?: keyof ItemMetadata | '*';
	groupBy?: string | string[];
	aggregate?: Aggregate;
	alias?: Record<string, string>;
};

export type Deep<T> = {
	[K in keyof SingleItem<T>]?: DeepQueryMany<SingleItem<T>[K]>;
};

export type DeepQueryMany<T> = {
	[K in keyof QueryMany<SingleItem<T>> as `_${string & K}`]: QueryMany<SingleItem<T>>[K];
};

export type SharedAggregate = {
	avg?: string[];
	avgDistinct?: string[];
	count?: string[];
	countDistinct?: string[];
	sum?: string[];
	sumDistinct?: string[];
	min?: string[];
	max?: string[];
};

export type Aggregate = {
	[K in keyof SharedAggregate]: string;
};

export type Sort<T> = (`${Extract<keyof SingleItem<T>, string>}` | `-${Extract<keyof SingleItem<T>, string>}`)[];

export type FilterOperators<T> = {
	_eq?: T;
	_neq?: T;
	_gt?: T;
	_gte?: T;
	_lt?: T;
	_lte?: T;
	_in?: T[];
	_nin?: T[];
	_between?: [T, T];
	_nbetween?: [T, T];
	_contains?: T;
	_ncontains?: T;
	_starts_with?: T;
	_nstarts_with?: T;
	_ends_with?: T;
	_nends_with?: T;
	_empty?: boolean;
	_nempty?: boolean;
	_nnull?: boolean;
	_null?: boolean;
	_intersects?: T;
	_nintersects?: T;
	_intersects_bbox?: T;
	_nintersects_bbox?: T;
};

export type LogicalFilterAnd<T> = { _and: Filter<T>[] };
export type LogicalFilterOr<T> = { _or: Filter<T>[] };
export type LogicalFilter<T> = LogicalFilterAnd<T> | LogicalFilterOr<T>;

export type FieldFilter<T> = {
	[K in keyof SingleItem<T>]?: FilterOperators<SingleItem<T>[K]> | FieldFilter<SingleItem<T>[K]>;
};

export type Filter<T> = LogicalFilter<T> | FieldFilter<T>;

export type ItemsOptions = {
	requestOptions: TransportRequestOptions;
};

type SingleItem<T> = Exclude<Single<T>, ID>;
type Single<T> = T extends Array<unknown> ? T[number] : T;

/**
 * CRUD at its finest
 */
export interface IItems<T extends Item> {
	createOne<Q extends QueryOne<T>>(item: PartialItem<T>, query?: Q, options?: ItemsOptions): Promise<OneItem<T, Q>>;
	createMany<Q extends QueryOne<T>>(
		items: PartialItem<T>[],
		query?: Q,
		options?: ItemsOptions
	): Promise<ManyItems<T, Q>>;

	readOne<Q extends QueryOne<T>>(id: ID, query?: Q, options?: ItemsOptions): Promise<OneItem<T, Q>>;
	readMany<Q extends QueryMany<T>>(ids: ID[], query?: Q, options?: ItemsOptions): Promise<ManyItems<T, Q>>;
	readByQuery<Q extends QueryMany<T>>(query?: Q, options?: ItemsOptions): Promise<ManyItems<T, Q>>;

	updateOne<Q extends QueryOne<T>>(
		id: ID,
		item: PartialItem<T>,
		query?: Q,
		options?: ItemsOptions
	): Promise<OneItem<T, Q>>;
	updateMany<Q extends QueryMany<T>>(
		ids: ID[],
		item: PartialItem<T>,
		query?: Q,
		options?: ItemsOptions
	): Promise<ManyItems<T, Q>>;

	deleteOne(id: ID, options?: ItemsOptions): Promise<void>;
	deleteMany(ids: ID[], options?: ItemsOptions): Promise<void>;
}

export class EmptyParamError extends Error {
	constructor(paramName?: string) {
		super(`${paramName ?? 'ID'} cannot be an empty string`);
	}
}

type IsUnion<T, U extends T = T> = T extends unknown ? ([U] extends [T] ? false : true) : false;
type IsObject<V> = V extends Record<string, any> ? true : false;
type AppendToPath<Path extends string, Appendix extends string> = Path extends '' ? Appendix : `${Path}.${Appendix}`;
type OneLevelUp<Path extends string> = Path extends `${infer Start}.${infer Middle}.${infer Rest}`
	? Rest extends `${string}.${string}.${string}`
		? `${Start}.${Middle}.${OneLevelUp<Rest>}`
		: Rest extends `${infer NewMiddle}.${infer _}`
		? `${Start}.${Middle}.${NewMiddle}`
		: Rest extends string
		? `${Start}.${Middle}`
		: ''
	: Path extends `${infer Start}.${infer _}`
	? Start
	: '';

type LevelsToAsterisks<Path extends string> = Path extends `${string}.${string}.${infer Rest}`
	? Rest extends `${string}.${string}.${string}`
		? `*.*.${LevelsToAsterisks<Rest>}`
		: Rest extends `${string}.${string}`
		? `*.*.*.*`
		: Rest extends string
		? `*.*.*`
		: ''
	: Path extends `${string}.${string}`
	? '*.*'
	: Path extends ''
	? ''
	: '*';

type DefaultAppends<Path extends string, Appendix extends string, Nested extends boolean = true> = Nested extends true
	? OneLevelUp<Path> extends ''
		?
				| AppendToPath<AppendToPath<LevelsToAsterisks<Path>, Appendix>, '*'>
				| AppendToPath<AppendToPath<LevelsToAsterisks<Path>, '*'>, '*'>
				| AppendToPath<AppendToPath<Path, Appendix>, '*'>
				| AppendToPath<Path, '*'>
		:
				| AppendToPath<AppendToPath<LevelsToAsterisks<Path>, Appendix>, '*'>
				| AppendToPath<AppendToPath<LevelsToAsterisks<Path>, '*'>, '*'>
				| AppendToPath<AppendToPath<Path, Appendix>, '*'>
				| AppendToPath<Path, '*'>
				| AppendToPath<AppendToPath<AppendToPath<OneLevelUp<Path>, '*'>, Appendix>, '*'>
				| AppendToPath<AppendToPath<OneLevelUp<Path>, '*'>, Appendix>
	: AppendToPath<Path, Appendix> | AppendToPath<LevelsToAsterisks<Path>, Appendix>;

type DotSeparated<
	T,
	N extends number,
	Level extends number[] = [],
	Path extends string = ''
> = Level['length'] extends N
	? Path
	: NonNullable<T> extends (infer U)[]
	? IsObject<U> extends true
		? DotSeparated<U, N, Level, Path>
		: Path
	: IsUnion<NonNullable<T>> extends true
	? DotSeparated<Extract<NonNullable<T>, Record<string, any>>, N, Level, Path>
	: IsObject<T> extends true
	? {
			[K in keyof T]: K extends string
				?
						| (NonNullable<T[K]> extends (infer U)[]
								? U extends Record<string, any>
									? DotSeparated<U, N, Level, AppendToPath<Path, K>> | DefaultAppends<Path, K>
									: DefaultAppends<Path, K, false>
								: IsUnion<T[K]> extends true
								? DotSeparated<T[K], N, [...Level, 0], AppendToPath<Path, K>>
								: IsObject<T[K]> extends true
								? DotSeparated<T[K], N, [...Level, 0], AppendToPath<Path, K>> | DefaultAppends<Path, K>
								: DefaultAppends<Path, K, false>)
						| DefaultAppends<Path, K, false>
				: never;
	  }[keyof T]
	: never;
