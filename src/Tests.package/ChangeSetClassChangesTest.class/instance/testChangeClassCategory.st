testing
testChangeClassCategory
	"Changing the class category for a class should result in a change
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
	self assert: saveClassDefinition = (ChangeSet current fatDefForClass: (Smalltalk globals classNamed: #JunkClass)).	"Redefine the class, changing only the class category"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-2'.	"Assert that the class definition has changed"
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
				equivalentTo:
					'Object subclass: #JunkClass
	instanceVariableNames: ''zzz ''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''DeleteMe-2''')