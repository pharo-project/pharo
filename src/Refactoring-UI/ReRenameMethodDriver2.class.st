"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'ReRenameMethodDriver2',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'originalMessage',
		'newMessage',
		'shouldEscape',
		'methodNameEditorPresenterClass',
		'oldName',
		'newName',
		'conflictingImplementors',
		'hasPrimitiveImplementor',
		'permutation'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenameMethodDriver2 class >> driverName [

	^ 'Rename method'
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> applicabilityPreconditionsAreSatisfied [

	^ self checkApplicabilityPreconditions isNil
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> browseOverrides [

	| overrides |
	overrides := conflictingImplementors violators.
	(Smalltalk tools toolNamed: #messageList)
		browse: (overrides collect: [ :ref | ref realClass lookupSelector: refactoring oldSelector ])
		asImplementorsOf: refactoring oldSelector
		
]

{ #category : 'execution' }
ReRenameMethodDriver2 >> changes [
	"Remember should not call generateChanges"
	^ refactoring renameChanges


]

{ #category : 'actions' }
ReRenameMethodDriver2 >> checkApplicabilityPreconditions [

	refactoring newName: newName.
	refactoring className: class .
	
	^ refactoring failedApplicabilityMessage
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> checkBehaviorPreconditions [

	| stream |
	
	conflictingImplementors := self refactoring superclassesDoNotDefineNewSelectorCondition .
	conflictingImplementors check ifTrue: [ ^ nil ].
	
	stream := String new writeStream .
	conflictingImplementors violationMessageOn: stream .
	^ stream contents 
]

{ #category : 'execution' }
ReRenameMethodDriver2 >> configureMessage [

	refactoring newSelector: newMessage selector.
	refactoring permutation: newMessage permutation
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> createAndSetNewRefactoring [

	super configureRefactoring .
	refactoring newName: newName.
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> hasFailedBehaviorPreservingPreconditions [
		
	^ refactoring failedBreakingChangePreconditions notEmpty 
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> hasPrimitiveImplementor [
    ^ hasPrimitiveImplementor
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> initializePrimitiveStatus [
    hasPrimitiveImplementor :=
        self refactoring implementors anySatisfy: [ :implementorClass |
            | method |
            method := implementorClass methodFor: self refactoring oldSelector.
            method notNil and: [ method compiledMethod isPrimitive ] ].
    ^ self
]

{ #category : 'configuration' }
ReRenameMethodDriver2 >> instantiateRefactoring [
	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."

	^ ReRenameMethodRefactoring new 
		renameMethod: originalMessage selector 
		in: class
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> isValidPermutation [

	^ refactoring areArgumentsPermuted
		and: [ refactoring validNewNameCondition check not ]
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> model: aModel renameMethodSignature: aMessage in: aClass [ 

	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> newName: aSymbol [

	newName := aSymbol
]

{ #category : 'accessing' }
ReRenameMethodDriver2 >> oldName: aString [ 
	oldName := aString
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> originalMessage [ 
	^ originalMessage 
]

{ #category : 'accessing' }
ReRenameMethodDriver2 >> permutation: aPermutation [
	permutation := aPermutation
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRenameMethodPreviewPresenter ]
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> renameMethod [

	self applyChanges 
]

{ #category : 'interaction' }
ReRenameMethodDriver2 >> requestNewMessage [
	| methodName parameters |

	parameters := originalMessage selector implementors
		ifNotEmpty: [ :m | m first argumentNames ]
		ifEmpty: [ refactoring buildParametersForSelector: originalMessage selector ].

	methodName := RBMethodName
		selector: originalMessage selector
		arguments: parameters.

	methodName permutation:
		(permutation ifNil: [ (1 to: parameters size) asArray ]).

	^ methodName
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> runRefactoring [

	| presenter | 
	
	super configureRefactoring .
	
	newMessage := self requestNewMessage.
	self initializePrimitiveStatus.
	
	presenter := (self previewPresenterClass newApplication: self application)
		scopes: scopes;
		driver: self;
		name: refactoring oldSelector asSymbol;
		arguments: newMessage arguments;
		openModal
	
	
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel renameMethodSignature: aMessage in: aClass 
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> setNewMessage: aSymbol [

	refactoring newSelector: aSymbol .
	newMessage selector: aSymbol .
	self configureMessage
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> setPermutation: aPermutation [

	newMessage permutation: aPermutation.
	self configureMessage
]
