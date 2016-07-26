accessing
renameTo: aSymbol
	"Rename a package with a different name, provided as a symbol"
	| oldName newName oldCategoryNames repositoryGroup |

	oldName := self name.
	newName := aSymbol asSymbol.
	repositoryGroup := self mcPackage workingCopy repositoryGroup.
	
	(self organizer includesPackageNamed: aSymbol)
		ifTrue: [ RPackageConflictError  signal: ('A package named {1} already exists' format: { aSymbol }) ].

	oldCategoryNames  := (self classTags collect: #categoryName as: Set) 
		add: self categoryName;
		yourself.
	self name: aSymbol.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self definedClasses do: [ :each | each category: newName, (each category allButFirst: oldName size) ].
		oldCategoryNames do: [ :each | SystemOrganizer default removeCategory: each ] ].
	self renameTagsPrefixedWith: oldName to: newName. 
	self renameExtensionsPrefixedWith: oldName to: newName.
			
	self organizer 
		basicUnregisterPackageNamed: oldName;
		basicRegisterPackage: self.
	
	self mcPackage workingCopy repositoryGroup: repositoryGroup. 
	
	SystemAnnouncer uniqueInstance 
		announce: (RPackageRenamed 
			to: self 
			oldName: oldName 
			newName: newName).