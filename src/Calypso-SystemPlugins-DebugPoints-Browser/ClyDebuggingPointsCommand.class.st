"
I am the superclass for all menu entries in the ""Debug Points"" menu entry when right clicking in a code presenter in Calypso.

All subclasses define a command to install a debug point with or without a behavior, on an AST node
"
Class {
	#name : 'ClyDebuggingPointsCommand',
	#superclass : 'ClyDebuggingCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'activation' }
ClyDebuggingPointsCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	^ 1.8
]

{ #category : 'testing' }
ClyDebuggingPointsCommand class >> isAbstract [
	^self == ClyDebuggingPointsCommand 
]

{ #category : 'activation' }
ClyDebuggingPointsCommand class >> methodContextMenuActivation [

	<classAnnotation>
	^ SycDebuggingPointsMenuActivation
		  byItemOf: ClyDebuggingPointsMenuGroup
		  order: self contextMenuOrder
		  for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
ClyDebuggingPointsCommand class >> methodEditorLeftBarMenuActivation [

	<classAnnotation>
	^ CmdTextLeftBarMenuActivation
		  byItemOf: ClyDebuggingPointsMenuGroup
		  order: self contextMenuOrder
		  for: ClyMethodSourceCodeContext
]

{ #category : 'activation' }
ClyDebuggingPointsCommand class >> sourceCodeMenuActivation [

	<classAnnotation>
	^ SycDebuggingPointsMenuActivation 
		  byItemOf: ClyDebuggingPointsMenuGroup
		  order: self contextMenuOrder
		  for: ClyMethodSourceCodeContext
]

{ #category : 'execution' }
ClyDebuggingPointsCommand >> putIconInProperties: dp [
	dp properties at: #Icon put: self defaultMenuIconName.
	DebugPointManager notifyDebugPointChanged: dp.
]
