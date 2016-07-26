api
browseVersionsForClass: aClass selector: aSelector 
	| method |
	method := (aClass compiledMethodAt: aSelector) ifNil: [ ^self ].
	
	^ self new
		browseVersionsOf: method
		