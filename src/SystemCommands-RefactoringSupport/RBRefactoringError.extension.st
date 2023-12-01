Extension { #name : 'RBRefactoringError' }

{ #category : '*SystemCommands-RefactoringSupport' }
RBRefactoringError >> failedConditions [
	^ failedConditions
]

{ #category : '*SystemCommands-RefactoringSupport' }
RBRefactoringError >> faledConditions: aCollection [
	"aCollection should contain objects of kind RBNewAbstractCondition"
	failedConditions := aCollection
]

{ #category : '*SystemCommands-RefactoringSupport' }
RBRefactoringError >> notifyUserOfCommand: aCommand [

	self uiManager alert: self messageText.
]
