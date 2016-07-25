accessing
characterBeforePosition: spanPosition ifNone: aBlock

	"Answer character which is right before the position, located in receiver
	or evaluate a block if there's none"
	^ characters at: spanPosition ifAbsent: aBlock
