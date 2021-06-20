"
I am a command to push down given methods
"
Class {
	#name : #SycPushDownMethodCommand,
	#superclass : #SysRefactoringMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycPushDownMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ SycMethodMenuActivation byRootGroupItemOrder: 20 for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
SycPushDownMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $p meta, $d meta for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycPushDownMethodCommand >> asRefactorings [
	"Return push down method refactoring using method"
	| refactoring |
	refactoring := RBPushDownMethodRefactoring 
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: methods first origin.
	^ OrderedCollection with: refactoring 
]

{ #category : #accessing }
SycPushDownMethodCommand >> defaultMenuIconName [ 
	^ #down
]

{ #category : #accessing }
SycPushDownMethodCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : #execution }
SycPushDownMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self selectMethods.
]

{ #category : #execution }
SycPushDownMethodCommand >> selectMethods [
	| dialog class|
	class := methods first origin.
	dialog := RBMethodsSelectionPresenter 
		   label: 'Push down methods from ', class name
			withItems: (class methods sort: [ :a :b | a asString < b asString ]) asOrderedCollection
			selecting: methods asOrderedCollection. 
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].
	methods := dialog presenter selectedItems.
]
