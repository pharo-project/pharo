initialization
readFrom: aStream
	"Read my official representation, 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens) from aStream"
	
	1 to: 4 do: [ :i | self at: i put: (self readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	5 to: 6 do: [ :i | self at: i put: (self readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	7 to: 8 do: [ :i | self at: i put: (self readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	9 to: 10 do: [ :i | self at: i put: (self readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	11 to: 16 do: [ :i | self at: i put: (self readHexByteFrom: aStream) ]