instance creation
subclassesOf: aClass referToSelector: aSelector 
	^self new 
		type: (Array 
				with: #subclassReferences
				with: aClass
				with: aSelector)
		block: 
			[(aClass subclasses detect: 
					[:each | 
					(each selectors detect: 
							[:sel | 
							| tree |
							tree := each parseTreeFor: sel.
							tree notNil and: [tree superMessages includes: aSelector]]
						ifNone: [nil]) notNil]
				ifNone: [nil]) notNil]
		errorString: '<1?:no:a> subclass of ' , aClass printString , ' refers to ' 
				, aSelector printString