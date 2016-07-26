printing
longPrintOn: aStream
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[self isReturnSpecial ifTrue:
			[^ aStream nextPutAll: 'Quick return ' , 
				(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255)].
		^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'].

	self isPrimitive ifTrue: [
		self printPrimitiveOn: aStream.
	].
	self symbolicBytecodes do: [ :each | each printOn: aStream ] separatedBy: [ aStream cr ]
