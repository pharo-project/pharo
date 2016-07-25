triggering
classParentOf: aClass renamedFrom: oldName to: newName
	self announce: (ClassParentRenamed classParentOf: aClass renamedFrom: oldName to: newName)