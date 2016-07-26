creating - silently
silentlyNewClassInCategory: category
	^ self
		silentlyNewSubclassOf: self defaultSuperclass
		instanceVariableNames: ''
		classVariableNames: ''
		category: category