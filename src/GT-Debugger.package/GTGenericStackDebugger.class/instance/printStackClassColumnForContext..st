printing/formatting
printStackClassColumnForContext: aContext
	| outer class methodClass |
	
	outer := aContext outerMostContext.
	^ String streamContents: [ :aStream |
		class := outer receiver class.
		aStream nextPutAll: class name.
		methodClass := outer methodClass.
		methodClass == class
			ifFalse:
				[ aStream 
						nextPut: $(;
						nextPutAll: methodClass name;
						nextPut: $) ] ]  