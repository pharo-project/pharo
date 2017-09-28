updating
updateClassLiteralKeysIn: aClass 
	aClass isMeta ifTrue: [ ^ self ].
	
	aClass methodsDo: [ :method | 
		method classBinding: (aClass environment bindingOf: aClass name) ]