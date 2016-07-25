private
methodForClass: aClassOrClassName selector: aSelector
	^ aClassOrClassName isClass 
		ifTrue: [ self methodForProvider: aClassOrClassName gtExamplesProvider selector: aSelector ]
		ifFalse: [ self methodForClassNamed: aClassOrClassName selector: aSelector ]