private
parse: bytes boundary: boundary binary: binary
	| next start fullBoundary |
	fullBoundary := '--' asByteArray, boundary.
	"Sometimes there is whitespace in front of the first boundary"
	start := (bytes
		indexOfSubCollection: fullBoundary
		startingAt: 1) + 1.
	next := bytes
		indexOfSubCollection: fullBoundary
		startingAt: start 
		ifAbsent: [ bytes size ].
	[ next < (bytes size - 2) ] whileTrue: [ | partReadStream |
		partReadStream := ReadStream on: bytes from: start + 2 to: next - 3.
		self addPart: (ZnMimePart perform: (binary ifTrue: [ #readBinaryFrom: ] ifFalse: [ #readFrom: ]) with: partReadStream).
		start := next + fullBoundary size.
		next := bytes
			indexOfSubCollection: fullBoundary
			startingAt: start
			ifAbsent: [ bytes size ] ]