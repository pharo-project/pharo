"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'ReRenameMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'class',
		'originalMessage',
		'newMessage',
		'command',
		'postAction',
		'shouldEscape',
		'newName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'private testing' }
ReRenameMethodDriver >> canAddArgs [
	^ false
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canEditName [
	^ true
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canRenameArgs [
	^ true
]

{ #category : 'execution' }
ReRenameMethodDriver >> changes [
	"Remember should not call generateChanges"
	^ refactoring renameChanges


]

{ #category : 'execution' }
ReRenameMethodDriver >> configureMessage [

	refactoring newSelector: newMessage selector.
	refactoring permutation: newMessage permutation.
	refactoring renameMap: newMessage renameMap.
]

{ #category : 'execution' }
ReRenameMethodDriver >> configureRefactoring [

	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	refactoring := ReRenameMethodRefactoring new renameMethod: originalMessage selector in: class 
]

{ #category : 'execution' }
ReRenameMethodDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'initialize' }
ReRenameMethodDriver >> model: aModel renameMethodSignature: aMessage in: aClass [ 
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'To be removed' }
ReRenameMethodDriver >> newMessage [

	^ newMessage
]

{ #category : 'To be removed' }
ReRenameMethodDriver >> postAction: aClosure [ 
	postAction := aClosure
]

{ #category : 'To be removed' }
ReRenameMethodDriver >> requestNewMessage [

	| methodName dialog  |
	
	methodName := RBMethodName
		              selector: originalMessage selector
		              arguments: originalMessage argumentNames.
	dialog := StMethodNameEditorPresenter
		          openOn: methodName
		          withInvalidArgs: (RBCondition invalidArgumentNamesForSelector: originalMessage selector in: model)
		          canRenameArgs: self canRenameArgs
		          canRemoveArgs: self canRemoveArgs
		          canAddArgs: self canAddArgs
		          canEditName: self canEditName.
	dialog ifNil: [ shouldEscape := true. ^ self ].
	"If the name is the same then there is no need to perform the refactoring 
	and in such case we can just escape."
	"note that it would be better to pass a method name to the refactoring instead of splitting tlla the arguments this way"
	refactoring newSelector: methodName selector; permutation: methodName permutation.
	refactoring areNamesTheSame 
		ifTrue: [ shouldEscape := true. ^ self ].
	"refactoring areArgumentsTheSame & refactoring areArgumentsPermuted not
		ifTrue: [ shouldEscape := true. ^ self ]."
	^ methodName
]

{ #category : 'execution' }
ReRenameMethodDriver >> runRefactoring [

	| failedConditions |
	self configureRefactoring.
	[ newMessage := self requestNewMessage.
	shouldEscape ifTrue: [ ^ self ].
	failedConditions := refactoring failedApplicabilityPreconditions.
	failedConditions isEmpty ]
		whileFalse: [
			failedConditions do: [ :cond |
				self application newInform
					label: cond errorString;
					title: 'Invalid name';
					openModal ] ].
	self halt.
	refactoring doesNotOverrideExistingMethodPrecondition check.
		"to revise"
	self configureMessage.
	self applyChanges.
]

{ #category : 'initialize' }
ReRenameMethodDriver >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel renameMethodSignature: aMessage in: aClass 
]
