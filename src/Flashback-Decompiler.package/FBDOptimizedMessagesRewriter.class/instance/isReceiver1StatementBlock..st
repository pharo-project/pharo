testing
isReceiver1StatementBlock: msgNode
	msgNode receiver isBlock
		ifFalse: [ ^ false ].
	^ msgNode receiver body statements size = 1