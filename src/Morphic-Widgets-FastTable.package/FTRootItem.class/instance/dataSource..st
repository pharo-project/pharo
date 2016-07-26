accessing
dataSource: anObject
	dataSource := anObject.
	children ifNotNil: [ children do: [ :item | item dataSource: anObject ] ]