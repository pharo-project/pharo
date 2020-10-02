Class {
	#name : #ClyCreateClassCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'package',
		'classTag'
	],
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #testing }
ClyCreateClassCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].
	
	^aBrowserContext isPackageSelected 
]

{ #category : #activation }
ClyCreateClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 4 for: ClyClassContextOfFullBrowser 
]

{ #category : #accessing }
ClyCreateClassCommand >> defaultMenuIconName [
	^#class
]

{ #category : #accessing }
ClyCreateClassCommand >> defaultMenuItemName [
	^'New class'
]

{ #category : #execution }
ClyCreateClassCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.
	
	package := aBrowserContext lastSelectedPackage.
	aBrowserContext isClassTagSelected ifTrue: [ 
		classTag := aBrowserContext lastSelectedClassTag]
]
