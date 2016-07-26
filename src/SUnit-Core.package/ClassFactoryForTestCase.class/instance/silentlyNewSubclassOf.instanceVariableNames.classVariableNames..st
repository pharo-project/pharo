creating - silently
silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString
	^ self
		silentlyNewSubclassOf: aClass
		instanceVariableNames: ivNamesString
		classVariableNames: classVarsString
		category: self defaultCategoryPostfix