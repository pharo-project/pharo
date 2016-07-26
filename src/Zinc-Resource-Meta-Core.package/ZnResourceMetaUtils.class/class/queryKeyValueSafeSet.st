accessing
queryKeyValueSafeSet
	"When a query is interpreted as a list of key=value&.. pairs,
	it is better to encode = and & and leave them out of the safe set.
	Furthermore, since + is interpreted as space in the query part,
	it is unsafe as well"
	
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$''()*,;:@/?'