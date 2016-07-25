updating
dataSource: anObject
	dataSource := anObject.
	children do: [ :item | item dataSource: anObject ]