"
This is the chapter of the refactoring help book about source code refactorings available from System Browsers code pane.
"
Class {
	#name : #RBBrowserSourceRefactoringHelp,
	#superclass : #RBBrowserSupportHelp,
	#category : #'Refactoring-Help'
}

{ #category : #accessing }
RBBrowserSourceRefactoringHelp class >> bookName [
	^ 'Source Refactorings'
]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> createCascadeRefactoring [
	^HelpTopic
		title: 'Create cascade'
		contents: RBCreateCascadeRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> extractMethodRefactoring [
	^HelpTopic
		title: 'Extract method'
		contents: RBExtractMethodRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> extractMethodToComponentRefactoring [
	^HelpTopic
		title: 'Extract method to component'
		contents: RBExtractMethodToComponentRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> extractToTemporaryRefactoring [
	^HelpTopic
		title: 'Extract to temporary'
		contents: RBExtractToTemporaryRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> inlineMethodFromComponentRefactoring [
	^HelpTopic
		title: 'Inline method from component'
		contents: RBInlineMethodFromComponentRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> inlineMethodRefactoring [
	^HelpTopic
		title: 'Inline method'
		contents: RBInlineMethodRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> inlineTemporaryRefactoring [
	^HelpTopic
		title: 'Inline temporary'
		contents: RBInlineTemporaryRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> moveTemporaryToInstvarRefactoring [
	^HelpTopic
		title: 'Move temporary to instvar'
		contents: RBTemporaryToInstanceVariableRefactoring comment.

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> moveVariableDefinitionRefactoring [
	^HelpTopic
		title: 'Move variable definition'
		contents: RBMoveVariableDefinitionRefactoring comment

]

{ #category : #accessing }
RBBrowserSourceRefactoringHelp class >> pages [
	^ #(createCascadeRefactoring extractMethodRefactoring extractMethodToComponentRefactoring extractToTemporaryRefactoring inlineMethodRefactoring inlineMethodFromComponentRefactoring inlineTemporaryRefactoring moveVariableDefinitionRefactoring renameTemporaryRefactoring splitCascadeRefactoring moveTemporaryToInstvarRefactoring)
]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> renameTemporaryRefactoring [
	^HelpTopic
		title: 'Rename temporary/parameter'
		contents: RBRenameTemporaryRefactoring comment

]

{ #category : #pages }
RBBrowserSourceRefactoringHelp class >> splitCascadeRefactoring [
	^HelpTopic
		title: 'Split cascade'
		contents: RBSplitCascadeRefactoring comment

]
