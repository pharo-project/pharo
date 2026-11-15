"
I am a base class for commands which modify signature of given message.

My subclasse should implement two methods: 

- createRefactoring 
It should return refactoring instance which will perform actual change.

- resultMessageSelector 
it should return target selector of the message after modification.

Internal Representation and Key Implementation Points.

    Instance Variables
	originalMessage:		<SycMessageDescription>
"
Class {
	#name : 'SycChangeMessageSignatureCommand',
	#superclass : 'ClyCommand',
	#traits : 'TRefactoringCommandSupport',
	#instVars : [
		'originalMessage',
		'refactoringScopes',
		'refactoredClass'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycChangeMessageSignatureCommand class >> isAbstract [
	^self = SycChangeMessageSignatureCommand
]

{ #category : 'execution' }
SycChangeMessageSignatureCommand >> applyCommandResult [

	super applyCommandResult.
	self context
		showMessage: originalMessage
		renamedTo: self resultMessageSelector asSymbol
]

{ #category : 'converting' }
SycChangeMessageSignatureCommand >> asRefactorings [
	^{self createRefactoring}
]

{ #category : 'requesting' }
SycChangeMessageSignatureCommand >> canAddArgs [
	^ true
]

{ #category : 'testing' }
SycChangeMessageSignatureCommand >> canBeExecuted [

	^ self context isMessageSelected | self context isMethodSelected
]

{ #category : 'testing' }
SycChangeMessageSignatureCommand >> canEditName [
	^ true
]

{ #category : 'requesting' }
SycChangeMessageSignatureCommand >> canRemoveArgs [
	^ true
]

{ #category : 'requesting' }
SycChangeMessageSignatureCommand >> canRenameArgs [
	^ true
]

{ #category : 'execution' }
SycChangeMessageSignatureCommand >> createRefactoring [
	self subclassResponsibility
]

{ #category : 'accessing' }
SycChangeMessageSignatureCommand >> orderInContext: aToolContext [

	^ aToolContext isMethodSourceContext
		ifTrue: [ 1.3 ]
		ifFalse: [ self order ]
]

{ #category : 'accessing' }
SycChangeMessageSignatureCommand >> originalMessage [
	^ originalMessage
]

{ #category : 'accessing' }
SycChangeMessageSignatureCommand >> originalMessage: anObject [
	originalMessage := anObject
]

{ #category : 'execution' }
SycChangeMessageSignatureCommand >> prepareFullExecution [

	self setUpModelFromContext: self context.
	refactoredClass := self context lastSelectedMessage methodClass.
	refactoringScopes := self context refactoringScopes.
	originalMessage := self context lastSelectedMessage
]

{ #category : 'accessing' }
SycChangeMessageSignatureCommand >> refactoredClass: aClass [ 
	refactoredClass := aClass
]

{ #category : 'requesting' }
SycChangeMessageSignatureCommand >> requestNewMessageIn: aToolContext [
	| methodName dialog invalidArgNames |
	
	invalidArgNames := self computeInvalidArgNamesForSelector: originalMessage selector.
	methodName := RBMethodName
		selector: originalMessage selector
		arguments: originalMessage argumentNames.
	
	dialog := (StMethodNameEditorPresenter newApplication: StPharoApplication current model: methodName)
		invalidArgNames: invalidArgNames;
		canRenameArgs: self canRenameArgs;
		canRemoveArgs: self canRemoveArgs;
		canAddArgs: self canAddArgs;
		canEditName: self canEditName;
		openModal.
	dialog cancelled ifTrue: [ CmCommandAborted signal ].

	((originalMessage selector = methodName selector) 
		and: [ originalMessage argumentNames = methodName arguments ]) 
		ifTrue: [ CmCommandAborted signal ].
	
	^ methodName
]

{ #category : 'execution' }
SycChangeMessageSignatureCommand >> resultMessageSelector [
	self subclassResponsibility
]
