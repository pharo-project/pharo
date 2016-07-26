accessing
protocol

	self isActive 
		ifTrue: [ ^ self compiledMethod ifNil: [ protocol ] ifNotNil: [ self compiledMethod protocol ]].
	self isHistorical
		ifTrue: [ ^ self protocolAtPointer ifNil: [ self compiledMethod ifNil: [ protocol ] ifNotNil:[ :cm | cm protocol ] ] ].
	^ protocol			