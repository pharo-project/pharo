Class {
	#name : #RBRefactoringChangeManagerTest,
	#superclass : #TestCase,
	#category : #'Refactoring-Tests-Changes'
}

{ #category : #tests }
RBRefactoringChangeManagerTest >> testChangeFactoryIsCorrectlyInitialized [ 

	RBRefactoryChangeManager nuke.
	self assert: RBRefactoryChangeManager changeFactory isNotNil
]
