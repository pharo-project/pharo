emulating
handleConflict: aConflict	
	|l r|
	l := #removed.
	r := #removed.
	aConflict localDefinition ifNotNil: [:d | l := d token].
	aConflict remoteDefinition ifNotNil: [:d | r := d token].	
	conflicts := conflicts copyWith: (Array with: r with: l).
	(l = #removed or: [r = #removed])
		ifTrue: [aConflict chooseRemote]
		ifFalse:
			[l > r
				ifTrue: [aConflict chooseLocal]
				ifFalse: [aConflict chooseRemote]]
		