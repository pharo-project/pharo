as yet unclassified
beforeMigratingClass: aBuilder installer: anInstaller.	
	aBuilder oldClass ifNotNil:[ :x | x traitComposition traits do: [:each | each removeUser: x]].
	aBuilder newClass traitComposition traits do: [:each | each addUser: aBuilder newClass].
		
	aBuilder oldMetaclass ifNotNil:[ :x | x traitComposition traits do: [:each | each removeUser: x]].
	aBuilder newMetaclass traitComposition traits do: [:each | each addUser: aBuilder newMetaclass]