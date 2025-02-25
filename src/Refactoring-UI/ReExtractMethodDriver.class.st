Class {
	#name : 'ReExtractMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'newMessage',
		'shouldEscape',
		'methodNameEditorPresenterClass',
		'body',
		'sourceMethod'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'testing' }
ReExtractMethodDriver >> canAddArgs [
	^ false
]

{ #category : 'testing' }
ReExtractMethodDriver >> canEditName [
	^ true
]

{ #category : 'testing' }
ReExtractMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : 'testing' }
ReExtractMethodDriver >> canRenameArgs [
	"for now false"
	^ false
]

{ #category : 'execution' }
ReExtractMethodDriver >> configureMessage [

	refactoring newSelector: newMessage selector.
	"refactoring permutation: newMessage permutation.
	refactoring renameMap: newMessage renameMap."
]

{ #category : 'resources' }
ReExtractMethodDriver >> configureRefactoring [

	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	refactoring := ReCompositeExtractMethodRefactoring new
		               extract: body
		               from: sourceMethod selector
		               in: class name.
	refactoring prepareForExecution 
]

{ #category : 'instance creation' }
ReExtractMethodDriver >> extract: aString from: compiledMethod in: aClass [

	class := aClass.
	sourceMethod := compiledMethod.
	body := aString
]

{ #category : 'execution' }
ReExtractMethodDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'initialization' }
ReExtractMethodDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'accessing' }
ReExtractMethodDriver >> methodNameEditorPresenterClass [

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter]
]

{ #category : 'accessing' }
ReExtractMethodDriver >> methodNameEditorPresenterClass: aClass [

	methodNameEditorPresenterClass := aClass
]

{ #category : 'accessing' }
ReExtractMethodDriver >> requestDialogWith: methodName [
	"This is lazy loaded and tests expect lazy loading, because they set `requestDialog`
	and thus side-step methodNameEditorPresenter"

	^ requestDialog ifNil: [
		  requestDialog := (self methodNameEditorPresenterClass
			                   openOn: methodName
			                   withInvalidArgs: (RBCondition
					                    invalidArgumentNamesForSelector:
					                    sourceMethod selector
					                    in: model)
			                   canRenameArgs: self canRenameArgs
			                   canRemoveArgs: self canRemoveArgs
			                   canAddArgs: self canAddArgs
			                   canEditName: self canEditName) presenter ]
]

{ #category : 'interaction' }
ReExtractMethodDriver >> requestNewMessage [

	| methodName dialog  |
	methodName := RBMethodName
		              selector: (refactoring selectorStartingFrom: 'f' argumentsSize: refactoring arguments size)
		              arguments: refactoring arguments.
	dialog := self requestDialogWith: methodName.
	dialog ifNil: [ shouldEscape := true. 
						^ self ].
	^ dialog methodName
]

{ #category : 'interaction' }
ReExtractMethodDriver >> runRefactoring [

	self configureRefactoring.
	newMessage := self requestNewMessage.
	refactoring newSelector: newMessage selector.
	self applyChanges 
]
