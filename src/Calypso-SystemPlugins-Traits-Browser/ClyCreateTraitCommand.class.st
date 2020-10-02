"
I am a command to create new trait in selected package.
I am a copy of command from Nautilus.
I just ask user to type trait definition in dialog window. Then I compile trait by evaluation of given definition.

Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<RPackage>
	classTag:		<Symcol>

"
Class {
	#name : #ClyCreateTraitCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'package',
		'classTag'
	],
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #testing }
ClyCreateTraitCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].
	
	^aBrowserContext isPackageSelected 
]

{ #category : #activation }
ClyCreateTraitCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 3 for: ClyClassContextOfFullBrowser 
]

{ #category : #accessing }
ClyCreateTraitCommand >> defaultMenuIconName [
	^#trait
]

{ #category : #accessing }
ClyCreateTraitCommand >> defaultMenuItemName [
	^'New trait'
]

{ #category : #execution }
ClyCreateTraitCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.
	
	package := aBrowserContext lastSelectedPackage.
	aBrowserContext isClassTagSelected ifTrue: [ 
		classTag := aBrowserContext lastSelectedClassTag]
]
