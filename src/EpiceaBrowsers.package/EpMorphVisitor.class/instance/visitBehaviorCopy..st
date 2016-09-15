visitor
visitBehaviorCopy: aClassCopy

	^ (self displayBehavior: aClassCopy definition name), (self displayOperation: 'copyNamed:'), (self displayBehavior: '#', aClassCopy newName)