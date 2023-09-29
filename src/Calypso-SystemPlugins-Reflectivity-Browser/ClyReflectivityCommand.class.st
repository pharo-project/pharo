"
I am a base class for commands which add/remove metalinks into given method or source node
"
Class {
	#name : 'ClyReflectivityCommand',
	#superclass : 'ClyDebuggingCommand',
	#classVars : [
		'ShowReflectivityCommands'
	],
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Commands',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyReflectivityCommand class >> canBeExecutedInContext: aBrowserContext [

	| node |
	^ self showReflectivityCommands and: [
		  self isAbstract not and: [
			  node := aBrowserContext lastSelectedSourceNode.
			  node isNotNil and: [
				  node isCommentNode not and: [
					  node isPragma not and: [ node isSequence not ] ] ] ] ]
]

{ #category : 'activation' }
ClyReflectivityCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	^ 8
]

{ #category : 'testing' }
ClyReflectivityCommand class >> isAbstract [
	^self == ClyReflectivityCommand
]

{ #category : 'activation' }
ClyReflectivityCommand class >> methodContextMenuActivation [

	<classAnnotation>
	^ SycReflectivityMenuActivation
		byItemOf: ClyReflectivityMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : 'accessing - defaults' }
ClyReflectivityCommand class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #reflectivity)
		label: 'Reflectivity';
		parent: #tools;
		description: 'Settings related to reflectivity tools';
		with: [
			(aBuilder setting: #showReflectivityCommands)
				label: 'Show reflectivity commands' ;
				default: false;
				target: self;
				description: 'Enable the usage of reflectivity commands (such as Metalinks) to instrument the code. These commands become visible in the contextual menu under the submenu ''Reflectivity''']
]

{ #category : 'accessing - defaults' }
ClyReflectivityCommand class >> showReflectivityCommands [

	^ ShowReflectivityCommands ifNil: [ ShowReflectivityCommands := false ]
]

{ #category : 'accessing - defaults' }
ClyReflectivityCommand class >> showReflectivityCommands: aBoolean [

	ShowReflectivityCommands := aBoolean
]

{ #category : 'activation' }
ClyReflectivityCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycReflectivityMenuActivation
		byItemOf: ClyReflectivityMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]
