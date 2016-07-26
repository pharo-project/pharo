private
silentlyRenameCategory: oldName toBe: newName

	self protocolOrganizer 
		renameProtocol: oldName
		into: newName.
