positioning
position
	"Return the receiver's current file position."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			^(self primGetPosition: fileID) - readLimit + position ] ].
	^self primGetPosition: fileID