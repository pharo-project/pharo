execute
execute
	| answer |
	answer := UIManager default 
				confirm: 'Are you sure you want to load ', target name.
	answer ifTrue: 
		[ target version load: target version defaultPackageNamesToLoad ]