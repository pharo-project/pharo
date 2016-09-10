tabs creation
redMorphTab
	^ (LockableTab
		label: 'Red Morph'
		icon: (self iconNamed: #smallBackIcon)
		morph:
		(Morph new
		color: Color red;
		yourself))
		closeable: false;
		yourself