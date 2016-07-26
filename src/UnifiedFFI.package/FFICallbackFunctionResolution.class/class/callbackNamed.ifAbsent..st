private
callbackNamed: functionName ifAbsent: aBlock 
	^ self callbackRegistry 
		at: functionName asSymbol
		ifAbsent: aBlock