Extension { #name : 'RBRefactoringError' }

{ #category : '*Commander-Core' }
RBRefactoringError >> notifyUserOfCommand: aCommand [

	InformativeNotification signal: messageText
]
