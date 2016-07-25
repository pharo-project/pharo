NOT CURRENTLY USED
primitiveFgetsInto: bufferPointer size: size file: file 
	"Not directly used for the moment but could be useful to test the reading without the
	SCAttachableFileStream using a code like this:
	
	buffer := NativeBoost allocate: 1001.
	(self primitiveFgetsInto: buffer  size: 1001 file: popenFile).
	self primitivePerror: 'fgets()'.
	buffer byteAt: 1001 put: 0.
	string := buffer readString.
	buffer free.
	string inspect. 
	
	"

	^ self ffiCall: #( String fgets(void* bufferPointer, int size, OSSCFile file) )
