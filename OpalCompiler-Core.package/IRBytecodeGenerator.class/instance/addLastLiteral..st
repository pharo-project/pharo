private
addLastLiteral: object
	lastLiteral ifNil: [ ^ lastLiteral := object ].
	((lastLiteral literalEqual: object)
		or: [ 
			"case of metaclass, they have no unique association"
			(lastLiteral isKindOf: Association) and: [ lastLiteral key isNil ] ])
		ifFalse: [ self error: 'there can only be one last literal' ]