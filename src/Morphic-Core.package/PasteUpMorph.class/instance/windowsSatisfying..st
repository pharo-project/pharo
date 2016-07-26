accessing
windowsSatisfying: windowBlock

	| windows |
	windows := OrderedCollection new.
	self submorphs do: [:m | 
		m embeddedWindowOrNil ifNotNil: [ 
			(windowBlock value: m) 
				ifTrue: [ windows addLast: m ] ] ].
		
	^ windows.		
		