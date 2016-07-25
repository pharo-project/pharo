printing
printOn: aStream 
	aStream nextPutAll: self class name;
		 nextPut: $(;
		 nextPutAll: contents asString;
		 nextPut: $,;
		 nextPutAll: self type;
		 nextPut: $)