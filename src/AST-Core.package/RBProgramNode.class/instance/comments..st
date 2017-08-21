accessing
comments: aCollection

	(aCollection ifNil: [#()])
		ifEmpty: [ self removeProperty: #comments ifAbsent: [] ] 
		ifNotEmpty: [ self propertyAt: #comments put: aCollection ].