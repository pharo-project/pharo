backward compatibility
renameCategory: oldName toBe: newName
	
	self 
		silentlyRenameCategory: oldName 
		toBe: newName.

	self notifyOfChangedCategoryFrom: oldName to: newName.
	"I need to notify also the selector changes, otherwise RPackage will not notice"
	(self protocolOrganizer protocolNamed: newName) methods 
		do: [ :each | self notifyOfChangedSelector: each from: oldName to: newName ]