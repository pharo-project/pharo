tests - class methods
testAllInstances
	self assert: UndefinedObject allInstances size = 1 description: 'There should be a single instance of UndefinedObject'.
	self assert: (UndefinedObject allInstances includes: nil) description: 'nil should be an instance of UndefinedObject'.