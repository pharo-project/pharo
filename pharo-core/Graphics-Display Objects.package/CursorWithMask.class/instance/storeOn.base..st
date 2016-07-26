mask
storeOn: aStream base: anInteger

	aStream nextPut: $(.
	super storeOn: aStream base: anInteger.
	aStream nextPutAll: ' setMaskForm: '.
	maskForm storeOn: aStream base: anInteger.
	aStream nextPut: $)