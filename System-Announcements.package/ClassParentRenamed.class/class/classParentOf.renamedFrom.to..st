instance creation
classParentOf: aClass renamedFrom: oldName to: newName
	^ self new
		parentOldName: oldName;
		parentNewName: newName;
		classAffected: aClass;
		yourself