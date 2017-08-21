navigating
/ aString
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].

	^self resolvePath: (self class from: aString)