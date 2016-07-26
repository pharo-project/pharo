test creation
createTestForClass: aClass
	| definition testClass className |
	aClass ifNil: [ ^ nil ].
	aClass isTestCase
		ifTrue: [ ^ nil ].
	aClass isMeta
		ifTrue: [ ^ nil ].
	className := self buildTestClassNameFrom: aClass.
	testClass := self class environment
		at: className
		ifPresent: [ :class | class ]
		ifAbsent:
			[ 
			definition := self buildTestClassDefinitionFrom: aClass.
			testClass := self
				compileANewClassFrom: definition
				notifying: nil
				startingFrom: self selectedClass.
			self class environment at: className.
			testClass
				comment:
					(self generateCommentForTestClass: testClass from: aClass) ].
	self selectedPackage: (self parentOfClass: testClass).
	self selectedClass: testClass.
	self updatePackageGroupAndClassList.
	^ testClass