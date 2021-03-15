Class {
	#name : #RBRemoveSenderMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #'failure tests' }
RBRemoveSenderMethodTest >> testIsDirectlyUsed [
	| refactoring |
	refactoring := RBRemoveSenderRefactoring
		remove: (58 to: 73)
		inMethod: #caller2
		forClass: RBRefactoryTestDataApp.
	self setupInlineExpressionFor: refactoring toReturn: false.
	self shouldFail: refactoring.
]

{ #category : #'failure tests' }
RBRemoveSenderMethodTest >> testNonExistantSelector [
	self shouldFail: (RBRemoveSenderRefactoring 
			remove: (14 to: 17)
			inMethod: #checkClass1:
			forClass: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBRemoveSenderMethodTest >> testRemoveSenderIntoCascadedMessage [
	"| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBRemoveSenderRefactoring 
			remove: (	74 to: 75)
			inMethod: #called:on:
			forClass: RBRefactoryTestDataApp.
			
	self setupInlineExpressionFor: refactoring toReturn: false.
	self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #called:on:) equals: (self parseMethod: 'called: anObject on: aBlock 
	| temp |
	temp := Transcript show: anObject printString.
	aBlock value')"
]

{ #category : #tests }
RBRemoveSenderMethodTest >> testRemoveSimpleSenderOfMessage [
	| refactoring |
	refactoring := RBRemoveSenderRefactoring 
			remove: (	40 to: 120)
			inMethod: #caller1
			forClass: RBRefactoryTestDataApp.
			
	self setupInlineExpressionFor: refactoring toReturn: false.
	self executeRefactoring: refactoring .

	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller1) equals: (self parseMethod: 'caller1
	| anObject |
	anObject := 5')
]
