menu & halo
dispatchKeystroke: anEvent
	anEvent keyCharacter == Character tab
		ifTrue: [ 
			self tabAmongFields
				ifTrue: [ ^ self tabHitWithEvent: anEvent ] ]