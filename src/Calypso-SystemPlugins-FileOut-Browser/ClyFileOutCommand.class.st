"
I am a command to file out given objects.
I simply call #fileOut for each of them.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	codeObjects:		<Colletion of<Object>>
"
Class {
	#name : #ClyFileOutCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'codeObjects'
	],
	#category : #'Calypso-SystemPlugins-FileOut-Browser'
}

{ #category : #activation }
ClyFileOutCommand class >> browserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClySystemBrowserContext
]

{ #category : #testing }
ClyFileOutCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext hasSelectedItems
]

{ #category : #accessing }
ClyFileOutCommand >> defaultMenuItemName [
	^'File Out'
]

{ #category : #execution }
ClyFileOutCommand >> execute [
	
	codeObjects do: [ :each | each fileOut ]
]

{ #category : #execution }
ClyFileOutCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	codeObjects := aToolContext selectedObjects
]
