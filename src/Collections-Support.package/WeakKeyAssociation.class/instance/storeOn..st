printing
storeOn: aStream
	"Store in the format (key->value)"
	aStream 
		nextPut: $(;
		nextPutAll: self class name.
	self expired ifTrue:[
		aStream nextPutAll: ' expired'
	] ifFalse: [
		aStream nextPutAll:' key: '.
		self key storeOn: aStream.
		aStream nextPutAll: ' value:'.
		value storeOn: aStream.
	].
	aStream nextPut: $)