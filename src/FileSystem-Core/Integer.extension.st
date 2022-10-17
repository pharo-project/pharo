Extension { #name : #Integer }

{ #category : #'*FileSystem-Core' }
Integer >> humanReadableSIByteSize [
	"Return the receiver as a string with SI binary (International System of Units) file size, e.g. '50 KB'"
	
	"(1000 * 1000 * 1000) humanReadableSIByteSize >>> '953 MB'"
	"(1024 * 1024 * 1024) humanReadableSIByteSize >>> '1 TB'"
	
	^ String streamContents: [ :s|
		self humanReadableSIByteSizeOn: s ]
]

{ #category : #'*FileSystem-Core' }
Integer >> humanReadableSIByteSizeOn: aStream [
	"Print a SI representation of myself on the argument. See humanReadableSIByteSize for better comment."
	
	| exponent base |
	base := 1000.
	self < base
		ifTrue: [ ^ aStream print: self; space; nextPut: $B ].
	exponent := (self log / base log) asInteger.
	(self / (base ** exponent)) printOn: aStream showingDecimalPlaces: 2.
	aStream
		space;
		nextPut: ('kMGTPE' at: exponent);
		nextPut: $B.
]
