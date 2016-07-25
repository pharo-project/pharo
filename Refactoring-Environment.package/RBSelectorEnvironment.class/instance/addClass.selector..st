adding
addClass: aClass selector: aSymbol 
	(aClass isMeta 
		ifTrue: [ metaClassSelectors at: aClass soleInstance name ifAbsentPut: [ IdentitySet new ] ]
		ifFalse: [ classSelectors at: aClass name ifAbsentPut: [ IdentitySet new ] ])
			add: aSymbol