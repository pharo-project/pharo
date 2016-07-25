focus handling
defaultFocusMorph

	^ self listMorph 
		ifNil: [ super defaultFocusMorph ]
		ifNotNil: [:morph | morph ]