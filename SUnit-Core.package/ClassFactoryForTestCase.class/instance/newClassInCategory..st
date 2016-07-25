creating
newClassInCategory: category
	^ self
		newSubclassOf: self defaultSuperclass
		instanceVariableNames: ''
		classVariableNames: ''
		category: category