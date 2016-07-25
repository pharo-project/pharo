instance creation
definesTemporaryVariable: aString in: aClass 
	| condition |
	condition := self new.
	condition 
		type: (Array with: #definesTempVar with: aClass with: aString)
		block: 
			[| method |
			method := self 
						methodDefiningTemporary: aString
						in: aClass
						ignore: [:class :selector | false].
			method notNil 
				ifTrue: 
					[condition errorMacro: method printString , ' defines variable ' , aString].
			method notNil]
		errorString: aClass printString 
				, ' <1?:does not >define<1?s:> temporary variable ' , aString.
	^condition