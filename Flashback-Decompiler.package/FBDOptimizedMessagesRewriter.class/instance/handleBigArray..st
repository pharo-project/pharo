handling
handleBigArray: msgNode
	| seq cascade |
	seq := msgNode parent parent.
	cascade := seq statements at: (seq statements indexOf: msgNode parent) +1 .
	seq removeNode: msgNode parent.
	seq replaceNode: cascade withNode: (RBArrayNode statements: (cascade  messages removeLast; collect: [ :msg | msg arguments first ]))