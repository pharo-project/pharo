acessing
nextBytecodeOffsetAfterJump
	"check if we are in ifTrue:ifFalse: / ifFalse:ifTrue: or in ifTrue: / ifFalse: and answers the next byte code offset"
	^destination last isJump 
		ifTrue: [ destination last destination first bytecodeOffset ]
		ifFalse: [ 
			(destination sequence at: ((destination size - 1) max: 1)) bytecodeOffset ]