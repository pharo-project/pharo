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
		'shouldEscape',
		'methodNameEditorPresenterClass'
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

{ #category : 'initialization' }
ReRenameMethodDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'for mocks' }
ReRenameMethodDriver >> methodNameEditorPresenterClass [

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter]
]

{ #category : 'for mocks' }
ReRenameMethodDriver >> methodNameEditorPresenterClass: aClass [

	methodNameEditorPresenterClass := aClass
]

{ #category : 'initialization' }
ReRenameMethodDriver >> model: aModel renameMethodSignature: aMessage in: aClass [ 
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'interaction' }
ReRenameMethodDriver >> requestNewMessage [

	| methodName dialog  |
	
	methodName := RBMethodName
		              selector: originalMessage selector
		              arguments: originalMessage argumentNames.
	dialog := self methodNameEditorPresenterClass
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
	refactoring newNameDoesNotRequireRefactoringPrecondition check
		ifTrue: [ shouldEscape := true. ^ self ].

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
				"We need to mockify it"
				self application newInform
					label: cond errorString;
					title: 'Invalid method name';
					openModal ] ].
	
	refactoring areNamesTheSame ifTrue: [ ^ self ].

	"names are different so we should check because this is not a permutation" 
	refactoring doesNotOverrideExistingMethodPreconditions 
		ifNotEmpty: [ 
			"We need to mockify it"
			| ok |
			ok := self application newConfirm
				 title: 'Watchout potential override detected!';
		  	    label: (String streamContents: 
							[:s | refactoring breakingChangeViolationMessageOn: s.
								s nextPutAll: '. Do you want the override(s)?']);
		  	    acceptLabel: 'Sure!';
		  	    cancelLabel: 'No, forget it';
		   	   openModal.
			ok ifFalse: [ ^ self ] ].
	self configureMessage.
	self applyChanges.
]

{ #category : 'initialization' }
ReRenameMethodDriver >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel renameMethodSignature: aMessage in: aClass 
]
