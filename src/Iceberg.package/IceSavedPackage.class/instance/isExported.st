querying
isExported
	| location |
	location := self location.
	^ location exists and: [ location hasChildren ]