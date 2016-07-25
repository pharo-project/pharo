handling
handleWhileTrue: msgNode
	| startTemp start end incrValue rcvr |
	(self isReceiverCorrectCondition: msgNode)
		ifFalse: [ ^ self ].
	rcvr := msgNode receiver body statements first.
	startTemp := rcvr receiver.
	start := (self extractStartValue: msgNode startTemp: startTemp) ifNil: [ ^ self ].
	end := rcvr arguments first.
	(self
		hasCorrectIncrInstruction: msgNode
		startTemp: startTemp
		isPositiveLoop: ((self isPositiveLoopOrNil: rcvr) ifNil: [ ^ self ]))
		ifFalse: [ ^ self ].
	incrValue := msgNode arguments first body statements last value arguments first.
	^ self
		convertWhileTrueToToByDo: msgNode
		start: start
		end: end
		by: incrValue
		incrVariable: startTemp
		assignement: (msgNode parent statements at: (msgNode parent statements indexOf: msgNode) - 1)