"
I am a command to push up given methods
"
Class {
	#name : #SycPushUpMethodCommand,
	#superclass : #SysRefactoringMethodCommand,
	#instVars : [
		'superclass'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycPushUpMethodCommand >> asRefactorings [
	| refactoring |
	refactoring :=  RBPullUpMethodRefactoring
		model: model
		pullUp: (methods collect: [ :each | each selector ])
		from: methods first origin.
	refactoring superClass: superclass.
	^ OrderedCollection with: refactoring
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuIconName [
	^ #up
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuItemName [
	^'Push up'
]

{ #category : #execution }
SycPushUpMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self selectMethodsAndSuperclass
]

{ #category : #execution }
SycPushUpMethodCommand >> selectMethodsAndSuperclass [
	| dialog class classes |
	class := methods first origin.
	classes := class allSuperclasses removeAllSuchThat: [ :each | each == Object or: [ each == ProtoObject ] ].
	dialog := RBSelectClassAndMethodsPresenter
			label: 'Methods to be pull up'
			dropLabel: 'Pull up methods of ', class name, ' to:'
			withItems: (class methods sort: [ :a :b | a asString < b asString ]) asOrderedCollection
			selecting: methods asOrderedCollection
			dropItems: classes
			acceptBlock: [ :item :items | superclass := item.
				methods := items ].
	dialog cancelled ifTrue: [ CmdCommandAborted signal ]
]
