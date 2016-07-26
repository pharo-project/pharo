instance creation
oldName: anOldName newName: aNewName class: aClass

	^ self basicNew 
		initializeOldName: anOldName newName: aNewName class: aClass;
		yourself