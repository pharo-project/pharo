filein/out
storeOn: aStream base: anInteger 
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."

	self unhibernate.
	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream crtab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream crtab: 1.
	aStream nextPutAll: 'depth: '.
	self depth printOn: aStream.
	aStream crtab: 1.
	aStream nextPutAll: 'fromArray: #('.
	self storeBitsOn:aStream base:anInteger.
	aStream nextPut: $).
	aStream crtab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).
