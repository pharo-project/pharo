initialization
initializePresenter

	models whenSelectedItemChanged: [ :class |
		protocols resetSelection.
		events resetSelection.
		class
			ifNil: [ 
				protocols items: #().
				events items: #() ]
			ifNotNil: [ 
				protocols items: (self methodsIn: class for: 'api').
				events items: (self methodsIn: class for: 'api-events') ] ].
	
	protocols whenSelectedItemChanged: [ :method | method ifNotNil: [ self resetEventSelection ] ].
	events whenSelectedItemChanged: [ :method | method ifNotNil: [ self resetProtocolSelection ] ].