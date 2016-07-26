tests
testFixObsoleteSharedPools
	| poolClass obsoletePoolName testClass preFixObsoleteClassNames postFixObsoleteClassNames |
	poolClass := classFactory newClass. "provides unique name over time via class variable counter"
	testClass := classFactory  
		newSubclassOf: classFactory defaultSuperclass 
		uses: { }
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: poolClass name asString
		category: classFactory defaultCategory.		
	classFactory deleteClass: poolClass.
	obsoletePoolName := poolClass name. 
	poolClass := nil.
	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	preFixObsoleteClassNames := SystemNavigation default obsoleteClasses collect: #name.
	self assert: (preFixObsoleteClassNames includes: obsoletePoolName).
	self assert: testClass sharedPoolNames size equals: 1.
	self assert: (testClass sharedPoolNames includes: obsoletePoolName ).
	self assert: (testClass sharedPoolsString beginsWith: 'AnObsolete').
	
	Smalltalk fixObsoleteSharedPools.
	
	postFixObsoleteClassNames := SystemNavigation default obsoleteClasses collect: #name.
	self deny: (postFixObsoleteClassNames includes: obsoletePoolName).
	self assert: testClass sharedPoolNames isEmpty.
	self assert: testClass sharedPoolsString isEmpty