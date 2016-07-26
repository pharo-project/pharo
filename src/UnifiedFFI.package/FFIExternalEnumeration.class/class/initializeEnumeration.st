class initialization
initializeEnumeration
	| unsigned |
	
	unsigned := true.
	self enumerationDictionary keysAndValuesDo: [ :eachName :eachValue | 
		| classBinding enumerationValue |
		
		(unsigned and: [ eachValue < 0 ]) 
			ifTrue: [ unsigned := false ].
		
		enumerationValue := self basicNew value: eachValue.
		classBinding := self classPool associationAt: eachName asSymbol ifAbsent: [ nil ].
		classBinding 
			ifNotNil: [ classBinding value: enumerationValue ]
			ifNil: [ 
				self classPool add: (ClassVariable 
					key: eachName asSymbol 
					value: enumerationValue ) ] ].
					
	representationType := unsigned
		ifTrue: [ FFIUInt32 new ]
		ifFalse: [ FFIInt32 new ]