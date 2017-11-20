"
This is the chapter of the refactoring help book about the method  refactoring available in the System Browser.
"
Class {
	#name : #RBBrowserMethodRefactoringHelp,
	#superclass : #RBBrowserSupportHelp,
	#category : #Refactoring-Help
}

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> addParameterRefactoring [
	^HelpTopic
		title: 'Add parameter'
		contents: RBAddParameterRefactoring comment

]

{ #category : #accessing }
RBBrowserMethodRefactoringHelp class >> bookName [
	^ 'Method Refactorings'
]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> deprecateMethodRefactoring [
	^HelpTopic
		title: 'Deprecate'
		contents: RBDeprecateMethodRefactoring comment

]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> inlineParameterRefactoring [
	^HelpTopic
		title: 'Inline parameter'
		contents: RBInlineParameterRefactoring comment

]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> inlineTargeSendsRefactoring [
	^HelpTopic
		title: 'Inline target sends'
		contents: RBInlineAllSendersRefactoring comment
]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> moveMethodRefactoring [
	^HelpTopic
		title: 'Move'
		contents: RBMoveMethodRefactoring comment

]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> moveToClassRefactoring [
	^HelpTopic
		title: 'Move to class side / instance side'
		contents: RBSwapMethodRefactoring comment
]

{ #category : #accessing }
RBBrowserMethodRefactoringHelp class >> pages [
	^ #(addParameterRefactoring deprecateMethodRefactoring inlineParameterRefactoring inlineTargeSendsRefactoring moveMethodRefactoring  moveToClassRefactoring pushUpMethodRefactoring pushDownpMethodRefactoring removeMethodRefactoring removeParameterRefactoring renameMethodRefactoring)
]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> pushDownpMethodRefactoring [
	^HelpTopic
		title: 'Push down'
		contents: RBPushDownMethodRefactoring comment

]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> pushUpMethodRefactoring [
	^HelpTopic
		title: 'Push up'
		contents: RBPullUpMethodRefactoring comment

]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> removeMethodRefactoring [
	^HelpTopic
		title: 'Remove'
		contents: RBRemoveMethodRefactoring comment

]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> removeParameterRefactoring [
	^HelpTopic
		title: 'Remove parameter'
		contents: RBRemoveParameterRefactoring comment
]

{ #category : #pages }
RBBrowserMethodRefactoringHelp class >> renameMethodRefactoring [
	^HelpTopic
		title: 'Rename method (all)'
		contents: RBRenameMethodRefactoring comment

]
