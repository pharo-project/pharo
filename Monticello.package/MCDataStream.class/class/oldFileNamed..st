instance creation
oldFileNamed: aString
	"Here is the way to use DataStream and ReferenceStream:

	|rr |
	rr := ReferenceStream oldFileNamed: 'test.obj'.
	^ rr nextAndClose.
"

	| strm ff |
	ff := FileStream oldFileOrNoneNamed: aString.
	ff ifNil: [^ nil].
	strm := self on: (ff binary).
	^ strm