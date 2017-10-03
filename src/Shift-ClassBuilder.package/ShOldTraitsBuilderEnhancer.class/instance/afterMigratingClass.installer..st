as yet unclassified
afterMigratingClass: aBuilder installer: anInstaller.	
	aBuilder newClass traitComposition traits do: [:each | each addUser: aBuilder newClass].
	aBuilder newMetaclass traitComposition traits do: [:each | each addUser: aBuilder newMetaclass]