private
getPluggableYellowButtonMenu: shiftKeyState
	| customMenu |
	^ (morph notNil and: [(customMenu := morph getMenu: shiftKeyState) notNil])
		ifTrue: [customMenu]
		ifFalse:
			[shiftKeyState
				ifTrue: [self class shiftedYellowButtonMenu]
				ifFalse: [self class yellowButtonMenu]]