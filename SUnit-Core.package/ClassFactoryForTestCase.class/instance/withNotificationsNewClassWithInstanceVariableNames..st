creating
withNotificationsNewClassWithInstanceVariableNames: instanceVariableNames
	^ self
		newSubclassOf: Object
		instanceVariableNames: instanceVariableNames
		classVariableNames: ''
		category: self defaultCategoryPostfix