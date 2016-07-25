instance creation
fileNamed: aString
	"Here is the way to use DataStream and ReferenceStream:

| rr |
rr := ReferenceStream fileNamed: 'test.obj'.
rr nextPut: 'Zork'.
rr close.
"

	^ self on: (FileStream fileNamed: aString)