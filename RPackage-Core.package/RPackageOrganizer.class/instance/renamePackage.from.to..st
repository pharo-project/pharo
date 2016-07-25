system integration
renamePackage: rPackage from: oldName to: newName 
	| classesAndProtocolsToRename |
	
	rPackage name: newName. 
	
	"we update the organizer"
	packages 
		at: newName 
		put: rPackage.
	packages 
		removeKey: oldName 
		ifAbsent: [ self reportBogusBehaviorOf:   #systemCategoryRenamedActionFrom: ]. 
	
	"we also rename all the extension protocols in the system with the new name"
	classesAndProtocolsToRename := rPackage extensionMethods asIdentitySet.
	classesAndProtocolsToRename 
		do: [:method | 
			method methodClass organization 
				silentlyRenameCategory: method category 
				toBe: '*', newName ]