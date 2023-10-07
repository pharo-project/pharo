"
I am a base class for commands which add/remove watchpoints and counters into given method or source node
"
Class {
	#name : 'ClyAdvancedDebuggingCommand',
	#superclass : 'ClyDebuggingCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Commands',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyAdvancedDebuggingCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	^ 1.8
]

{ #category : 'testing' }
ClyAdvancedDebuggingCommand class >> isAbstract [
	^self == ClyAdvancedDebuggingCommand
]

{ #category : 'activation' }
ClyAdvancedDebuggingCommand class >> methodContextMenuActivation [

	<classAnnotation>
	^ SycAdvancedDebuggingMenuActivation
		  byItemOf: ClyAdvancedDebuggingMenuGroup
		  order: self contextMenuOrder
		  for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
ClyAdvancedDebuggingCommand class >> methodEditorLeftBarMenuActivation [

	<classAnnotation>
	^ CmdTextLeftBarMenuActivation
		  byItemOf: ClyAdvancedDebuggingMenuGroup
		  order: self contextMenuOrder
		  for: ClyMethodSourceCodeContext
]

{ #category : 'activation' }
ClyAdvancedDebuggingCommand class >> sourceCodeMenuActivation [

	<classAnnotation>
	^ SycAdvancedDebuggingMenuActivation
		  byItemOf: ClyAdvancedDebuggingMenuGroup
		  order: self contextMenuOrder
		  for: ClyMethodSourceCodeContext
]
