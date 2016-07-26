testing
testAddInstanceVariable
	"Adding an instance variable to the class should result in a change
	record being added to the current change set."

	| saveClassDefinition |
	"Define a class and save its definition"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.
	saveClassDefinition := (Smalltalk globals classNamed: #JunkClass) definition.
	self
		assert:
			(self
				isDefinition: saveClassDefinition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).	"Redefine the class, adding one instance variable"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz aaa'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.	"Assert that the class definition has changed"
	self deny: (self isDefinition: (Smalltalk globals classNamed: #JunkClass) definition equivalentTo: saveClassDefinition).
	self
		deny:
			(self
				isDefinition: saveClassDefinition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).
	self
		assert:
			(self
				isDefinition: (Smalltalk globals classNamed: #JunkClass) definition
				equivalentTo: (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass))).	"Assert that the change has been recorded in the current change set"
	self
		assert:
			(self
				isDefinition: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) priorDefinition
				equivalentTo: saveClassDefinition)