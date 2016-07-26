tabs creation
redMorphTab
	^ (LockableTab
		label: 'Red Morph'
		icon: (Smalltalk ui icons iconNamed: #smallBackIcon)
		morph:
		(Morph new
		color: Color red;
		yourself))
		closeable: false;
		yourself