as yet unclassified
rowAt: aLevel
	rows ifNil: [  rows := OrderedCollection new ].
	
	[ aLevel > rows size ] whileTrue: [  rows add: OrderedCollection new ].

	^ rows at: aLevel