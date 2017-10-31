migrating
beforeMigratingClass: aBuilder installer: anInstaller.	
	aBuilder oldClass ifNotNil:[ :x | x traitComposition traits do: [:each | each removeUser: x]].	
	aBuilder oldMetaclass ifNotNil:[ :x | x traitComposition traits do: [:each | each removeUser: x]].
