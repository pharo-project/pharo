printing
storeOn: aStream 
	"Refer to the comment in Object|storeOn:."

	aStream nextPut: $(;
	 nextPutAll: self class name;
	 nextPutAll: ' selector: ';
	 store: selector;
	 nextPutAll: ' arguments: ';
	 store: args;
	 nextPut: $)