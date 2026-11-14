"
I am the shortcut of a Commander2 command. I keep the keyboard shortcut annotation of the v1 CmdShortcutActivation, and when the key combination completely matches I run the command through the Commander2 contract (prepare, execute, apply the result, report failures to the context). I am removed together with Commander v1.
"
Class {
	#name : 'ClyShortcutActivation',
	#superclass : 'ClyCommandActivationStrategy',
	#instVars : [
		'keyCombination'
	],
	#category : 'Commander-Calypso-Compatibility',
	#package : 'Commander-Calypso-Compatibility'
}

{ #category : 'instance creation' }
ClyShortcutActivation class >> by: aKeyCombination for: anAnnotationUser [
	^(self for: anAnnotationUser)
		keyCombination: aKeyCombination
]

{ #category : 'command registry' }
ClyShortcutActivation class >> interactionName [

	^ #shortcuts
]

{ #category : 'well known shortcuts' }
ClyShortcutActivation class >> moveFor: anAnnotationUser [
	<classAnnotationDependency>

	^self by: $o meta for: anAnnotationUser
]

{ #category : 'well known shortcuts' }
ClyShortcutActivation class >> removalFor: anAnnotationUser [
	<classAnnotationDependency>

	^self by: $x meta for: anAnnotationUser
]

{ #category : 'well known shortcuts' }
ClyShortcutActivation class >> renamingFor: anAnnotationUser [
	<classAnnotationDependency>

	^self by: $r meta for: anAnnotationUser
]

{ #category : 'accessing' }
ClyShortcutActivation >> action [
	"Answer a <CompiledMethod> defining the receiver's action"

	^ self annotatedClass class lookupSelector: self declarationSelector
]

{ #category : 'printing' }
ClyShortcutActivation >> description [
	"Answer a <String> describing the receiver. We use the `action` method comments to retrieve the description"

	^ self action comment
		ifNil: [ String empty ]
		ifNotNil: [ : comm | comm ]
]

{ #category : 'executing' }
ClyShortcutActivation >> executeCommandInContext: aToolContext [

	self commandClass new
		context: aToolContext;
		execute
]

{ #category : 'accessing' }
ClyShortcutActivation >> keyCombination [
	^ keyCombination
]

{ #category : 'accessing' }
ClyShortcutActivation >> keyCombination: anObject [
	keyCombination := anObject
]

{ #category : 'printing' }
ClyShortcutActivation >> name [

	^ self declarationSelector asString
]

{ #category : 'printing' }
ClyShortcutActivation >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	keyCombination printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'printing' }
ClyShortcutActivation >> scope [
	"The receiver's scope is the package name where is installed"

	^ self annotatedClass packageName
]

{ #category : 'printing' }
ClyShortcutActivation >> scopeName [

	^ self scope asString
]

{ #category : 'printing' }
ClyShortcutActivation >> shortcut [
	"Answer a <String> representation of the receiver's key combination"

	^ self keyCombination asString
]

{ #category : 'executing' }
ClyShortcutActivation >> tryExecuteCommandInContext: aToolContext byEvents: anEventBuffer [

	(keyCombination matches: anEventBuffer) ifFalse: [ ^ false ].

	(self isActiveInContext: aToolContext) ifFalse: [ ^ false ].

	(keyCombination matchesCompletely: anEventBuffer)
		ifTrue: [
				KMBuffer uniqueInstance completeMatch.
				self executeCommandInContext: aToolContext ]
		ifFalse: [ KMBuffer uniqueInstance partialMatch ].
	^ true
]
