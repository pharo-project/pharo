"
I am a command to request a protocol from the user which should be used to reclassify a method
"
Class {
	#name : 'ClyRequestProtocolForEditorCommand',
	#superclass : 'ClyMethodEditorCommand',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'activation' }
ClyRequestProtocolForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $e meta, $p meta for: ClyTextEditorContext
]

{ #category : 'accessing' }
ClyRequestProtocolForEditorCommand >> description [

	^ 'Requests a protocol to reclassify a method.'
]

{ #category : 'execution' }
ClyRequestProtocolForEditorCommand >> executeRefactoring [
	| protocolEditor |
	protocolEditor := methodEditor protocolAndPackageEditor.
	protocolEditor requestChangeBy: [ protocolEditor requestProtocol]
]

{ #category : 'accessing' }
ClyRequestProtocolForEditorCommand >> name [

	^ self context tool hasUnacceptedEdits
		ifTrue: [ 'Classify currently editing method' ]
		ifFalse: [ 'Classify method' ]
]

{ #category : 'accessing' }
ClyRequestProtocolForEditorCommand >> order [

	^ 1000
]
