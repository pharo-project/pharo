Class {
	#name : 'RBRefactoringChangeManagerTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'tests' }
RBRefactoringChangeManagerTest >> testChangeFactoryIsCorrectlyInitialized [

	RBRefactoryChangeManager nuke.
	self assert: RBRefactoryChangeManager changeFactory isNotNil
]
