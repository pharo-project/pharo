Extension { #name : #MiniWorld }

{ #category : #'*OSWindow-Core' }
MiniWorld >> open [

	| worldRenderer |
	worldRenderer := OSWorldRenderer forWorld: self.
	worldRenderer windowCloseAction: [ self worldRenderer deactivate ].
	worldState activeHand: (self addHand: HandMorph new).
	WorldMorph addExtraWorld: self.
	worldRenderer osWindow announcer weak
		when: DeleteWindowByCrossAnnouncement
		send: #deleteByCross
		to: self
]

{ #category : #'*OSWindow-Core' }
MiniWorld >> openWithAttributes: aAttributes [

	| worldRenderer |
	worldRenderer := OSWorldRenderer
		                 forWorld: self
		                 withAttributs: aAttributes.
	self worldState worldRenderer:
		(worldRenderer windowCloseAction: [ self worldRenderer deactivate ]).
	worldState activeHand: (self addHand: HandMorph new).
	WorldMorph addExtraWorld: self.
	worldRenderer osWindow announcer weak
		when: DeleteWindowByCrossAnnouncement
		send: #deleteByCross
		to: self
]
