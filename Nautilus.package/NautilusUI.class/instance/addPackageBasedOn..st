package
addPackageBasedOn: aPackage
	[ ^ super addPackageBasedOn: aPackage ]
	on: RPackageConflictError 
	do: [ :e | UIManager default inform: e messageText, '. You need to use promote/demote options instead.' ].
	
	^ nil