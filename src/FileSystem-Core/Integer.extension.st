Extension { #name : #Integer }

{ #category : #'*FileSystem-Core' }
Integer >> humanReadableSIByteSize [
	^ String streamContents: [ :s|
		self humanReadableSIByteSizeOn: s ]
]

{ #category : #'*FileSystem-Core' }
Integer >> humanReadableSIByteSizeOn: s [
	| exponent base |
	"Print a string with an SI binary unit represation of myself."
	base := 1000.
	self < base
		ifTrue: [ ^ s print: self; space; nextPut: $B ].
	exponent := (self log / base log) asInteger.
	(self / (base ** exponent)) printOn: s showingDecimalPlaces: 2.
	s 
		space;
		nextPut: ('kMGTPE' at: exponent);
		nextPut: $B.
]
