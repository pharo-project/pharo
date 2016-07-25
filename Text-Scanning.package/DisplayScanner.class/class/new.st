instance creation
new
	"Use default concrete class"
	^(self == DisplayScanner
		ifTrue: [BitBltDisplayScanner]
		ifFalse: [self]) basicNew initialize