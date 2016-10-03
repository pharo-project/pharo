testing
testChangeClassCategoryAddsNewChangeRecord
	"Changing the class category for a class should result in a change
	record being updated in the current change set."

	"At the start of this test, JunkClass should not exist, and there should be
	no change records pertaining to it in the change set."

	self deny: (Smalltalk hasClassNamed: 'JunkClass').
	self assert: (ChangeSet current changeRecorderFor: (Smalltalk globals classNamed: #JunkClass)) thisName = nil.	"Remove bogus change records created as side effect of preceding assert"
	ChangeSet current removeClassChanges: 'nil'.	"Define a class and save its definition"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-1'.	"Forget about JunkClass in the change set"
	ChangeSet current removeClassChanges: 'JunkClass'.	"Redefine the class, changing only the class category"
	Object
		subclass: #JunkClass
		instanceVariableNames: 'zzz'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'DeleteMe-2'.	"A change record should now exist in the change set"
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