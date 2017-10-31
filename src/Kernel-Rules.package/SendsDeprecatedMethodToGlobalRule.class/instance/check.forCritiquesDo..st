running
check: aNode forCritiquesDo: aCriticBlock
	| messageReceiver |

	aNode isMessage ifFalse: [ ^ self ].
	
	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ self ].
	messageReceiver isGlobal ifFalse: [ ^ self ].
	(self
		check: aNode selector
		forDeprecationIn: messageReceiver name) ifFalse: [ ^ self ].
		
	self critiqueFor: aNode