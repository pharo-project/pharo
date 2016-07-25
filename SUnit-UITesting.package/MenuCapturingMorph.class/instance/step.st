stepping and presenter
step

	menu := World submorphs detect: [ :m | m isKindOf: MenuMorph ] ifNone: [ menu ].
	menu isNil ifFalse: [ 
		menu delete.
		self delete ].