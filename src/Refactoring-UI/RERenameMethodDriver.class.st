"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'RERenameMethodDriver',
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
RERenameMethodDriver >> canAddArgs [
	^ false
]

{ #category : 'private testing' }
RERenameMethodDriver >> canEditName [
	^ true
]

{ #category : 'private testing' }
RERenameMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : 'private testing' }
RERenameMethodDriver >> canRenameArgs [
	^ true
]

{ #category : 'execution' }
RERenameMethodDriver >> changes [
	"Remember should not call generateChanges"
	^ refactoring renameChanges


]

{ #category : 'execution' }
RERenameMethodDriver >> configureMessage [

	refactoring newSelector: newMessage selector.
	refactoring permutation: newMessage permutation.
	refactoring renameMap: newMessage renameMap.
]

{ #category : 'execution' }
RERenameMethodDriver >> configureRefactoring [

	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	refactoring := RERenameMethodRefactoring new renameMethod: originalMessage selector in: class 
]

{ #category : 'execution' }
RERenameMethodDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'initialize' }
RERenameMethodDriver >> model: aModel renameMethodSignature: aMessage in: aClass [ 
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'To be removed' }
RERenameMethodDriver >> newMessage [

	^ newMessage
]

{ #category : 'To be removed' }
RERenameMethodDriver >> postAction: aClosure [ 
	postAction := aClosure
]

{ #category : 'To be removed' }
RERenameMethodDriver >> requestNewMessage [

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
RERenameMethodDriver >> runRefactoring [

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
RERenameMethodDriver >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel renameMethodSignature: aMessage in: aClass 
]
