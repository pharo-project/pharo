accessing
categoryNamed: aString 
	^ categories 
		detect: [ :each | each name = aString ]
		ifNone: [ NotFound signalFor: aString  ]