"
I am a base class for commands which add/remove metalinks into given method or source node
"
Class {
	#name : #ClyReflectivityCommand,
	#superclass : #ClyDebuggingCommand,
	#classVars : [
		'ShowReflectivityCommands'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #activation }
ClyReflectivityCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	^ 8
]

{ #category : #testing }
ClyReflectivityCommand class >> isAbstract [
	^self == ClyReflectivityCommand 
]

{ #category : #activation }
ClyReflectivityCommand class >> methodContextMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation 
		byItemOf: ClyReflectivityMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : #'accessing - defaults' }
ClyReflectivityCommand class >> showReflectivityCommands [

	^ ShowReflectivityCommands ifNil: [ ShowReflectivityCommands := false ]
]

{ #category : #'accessing - defaults' }
ClyReflectivityCommand class >> showReflectivityCommands: aBoolean [

	ShowReflectivityCommands := aBoolean
]

{ #category : #activation }
ClyReflectivityCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycReflectivityMenuActivation 
		byItemOf: ClyReflectivityMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]
