instance creation
definesTempVar: aString in: aClass ignoreClass: subclass 
	| condition |
	condition := self new.
	condition 
		type: (Array with: #definesTempVarIgnoring with: aClass with: aString with: subclass)
		block: 
			[| method |
			method := self 
						methodDefiningTemporary: aString
						in: aClass
						ignore: [:class :aSelector | class includesClass: subclass].
			method notNil 
				ifTrue: 
					[condition errorMacro: method printString , ' defines variable ' , aString].
			method notNil]
		errorString: aClass printString 
				, ' <1?:does not >define<1?s:> temporary variable ' , aString.
	^condition