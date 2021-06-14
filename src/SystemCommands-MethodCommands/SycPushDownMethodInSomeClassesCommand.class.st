Class {
	#name : #SycPushDownMethodInSomeClassesCommand,
	#superclass : #SysRefactoringMethodCommand,
	#instVars : [
		'classes'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycPushDownMethodInSomeClassesCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ SycMethodMenuActivation byRootGroupItemOrder: 21 for: ClyMethod asCalypsoItemContext
]

{ #category : #converting }
SycPushDownMethodInSomeClassesCommand >> asRefactorings [
	"Return push down method refactoring using method"
	| refactoring |
	refactoring := RBPushDownMethodRefactoring 
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: methods first origin
		in: self classes.
	^ OrderedCollection with: refactoring 
]

{ #category : #converting }
SycPushDownMethodInSomeClassesCommand >> classes [
	^ classes ifNil: [ classes := #() ]
]

{ #category : #accessing }
SycPushDownMethodInSomeClassesCommand >> defaultMenuIconName [ 
	^ #down
]

{ #category : #accessing }
SycPushDownMethodInSomeClassesCommand >> defaultMenuItemName [
	^'Push down in some classes'
]

{ #category : #execution }
SycPushDownMethodInSomeClassesCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self selectMethodsAndClasses.
]

{ #category : #execution }
SycPushDownMethodInSomeClassesCommand >> selectMethodsAndClasses [
	|selectedMethods selectedClasses dialog class|
	class := methods first origin.
	selectedMethods := methods copy asOrderedCollection.
	selectedClasses := class subclasses.
	dialog := RBMethodsSelectionPresenter 
		   label: 'Push down methods from ', class name
			withItems: (class methods sort: [ :a :b | a asString < b asString ]) asOrderedCollection
			selecting: selectedMethods. 
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].
	methods := dialog presenter selectedItems.
	dialog := RBVariablesSelectionPresenter 
		   label: 'Push down in classes: '
			withItems: (class subclasses sort: [ :a :b | a name < b name ]) asOrderedCollection
			selecting: selectedClasses. 
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].
	classes := (dialog presenter selectedItems) collect: #name.
]
