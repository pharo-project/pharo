testing
isReceiverCorrectCondition: msgNode
	| condition |
	(self isReceiver1StatementBlock: msgNode ) ifFalse: [ ^ false ].
	condition := msgNode receiver body statements first.
	^ condition isMessage and: [ condition isBinary ]