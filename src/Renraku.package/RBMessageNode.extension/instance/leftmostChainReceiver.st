*renraku
leftmostChainReceiver

	^ self receiver isMessage
		ifTrue: [ self receiver receiver ]
		ifFalse: [ self receiver ]