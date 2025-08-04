"
I describe how access and execute command using given shortcut.

Add me to commands using:

	YourCommand>>yourApplicationShortcutActivation
		<classAnnotation>
		^CmdShortcutCommandActivation by: $y meta for: YourAppContext

I also define standard shortcuts on class side for rename and remove commands:

- renamingFor: aToolContext
- removalFor: aToolContext

In addition I add to the settings browser the root group ""Shortcuts"" with all my registered instances.
So user can redefine default values in settings browser. I use class annotation redefinition mehanizm to support it. 
To reset all redefined values evaluate following expression:

	CmdShortcutCommandActivation revertRedefinedInstances. 

Internal Representation and Key Implementation Points.

    Instance Variables
	keyCombination:		<KKKeyCombination>
"
Class {
	#name : 'CmdShortcutActivation',
	#superclass : 'CmdCommandActivationStrategy',
	#instVars : [
		'keyCombination'
	],
	#category : 'Commander-Activators-Shortcut',
	#package : 'Commander-Activators-Shortcut'
}

{ #category : 'instance creation' }
CmdShortcutActivation class >> by: aKeyCombination for: anAnnotationUser [
	^(self for: anAnnotationUser)
		keyCombination: aKeyCombination
]

{ #category : 'well known shortcuts' }
CmdShortcutActivation class >> removalFor: anAnnotationUser [
	<classAnnotationDependency>

	^self by: $x meta for: anAnnotationUser
]

{ #category : 'well known shortcuts' }
CmdShortcutActivation class >> renamingFor: anAnnotationUser [
	<classAnnotationDependency>

	^self by: $r meta for: anAnnotationUser
]

{ #category : 'accessing' }
CmdShortcutActivation >> action [
	"Answer a <CompiledMethod> defining the receiver's action"

	^ self annotatedClass class lookupSelector: self declarationSelector
]

{ #category : 'printing' }
CmdShortcutActivation >> description [
	"Answer a <String> describing the receiver. We use the `action` method comments to retrieve the description"

	^ self action comment
		ifNil: [ String empty ]
		ifNotNil: [ : comm | comm ]
]

{ #category : 'accessing' }
CmdShortcutActivation >> keyCombination [
	^ keyCombination
]

{ #category : 'accessing' }
CmdShortcutActivation >> keyCombination: anObject [
	keyCombination := anObject
]

{ #category : 'printing' }
CmdShortcutActivation >> name [

	^ self declarationSelector asString
]

{ #category : 'printing' }
CmdShortcutActivation >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	keyCombination printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'printing' }
CmdShortcutActivation >> scope [
	"The receiver's scope is the package name where is installed"

	^ self annotatedClass packageName
]

{ #category : 'printing' }
CmdShortcutActivation >> scopeName [

	^ self scope asString
]

{ #category : 'printing' }
CmdShortcutActivation >> shortcut [
	"Answer a <String> representation of the receiver's key combination"

	^ self keyCombination asString
]

{ #category : 'command execution' }
CmdShortcutActivation >> tryExecuteCommandInContext: aToolContext byEvents: anEventBuffer [

	| activator |
	(keyCombination matches: anEventBuffer) ifTrue: [
		(self isActiveInContext: aToolContext) ifFalse: [ ^false].

		(keyCombination matchesCompletely: anEventBuffer)
			ifTrue: [ KMBuffer uniqueInstance completeMatch.
				activator := self newActivatorFor: aToolContext.
				activator executeCommand]
			ifFalse: [ KMBuffer uniqueInstance partialMatch].
		^true].
	^false
]
