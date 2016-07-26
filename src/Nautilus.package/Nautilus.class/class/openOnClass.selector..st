opening
openOnClass: aClass selector: aSelector 

	^ aSelector
		ifNil: [ self openOnClass: aClass ]
		ifNotNil: [| method |
			method := aClass methodDict at: aSelector ifAbsent: [ ^ self openOnClass:aClass ].
			method ifNotNil: [ self openOnMethod: method ]]