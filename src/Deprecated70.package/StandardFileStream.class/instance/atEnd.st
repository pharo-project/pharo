testing
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	^self primAtEnd: fileID