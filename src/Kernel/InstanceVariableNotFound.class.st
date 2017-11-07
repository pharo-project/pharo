"
Description
--------------------------

I am an exception raised when we try to access to an instance variable by its name but the receiver does not know this instance variable.

Examples 
--------------------------

	InstanceVariableNotFound signalFor: 'test'.
	
	Object new instVarNamed: 'test'.
	
	[ Object new instVarNamed: 'test' ] on: InstanceVariableNotFound do: [ :ex | ex ].

Internal Representation and Key Implementation Points.
--------------------------

    Instance Variables
	instVarName:		<aString>	Name of the instance variable we tried to access.
"
Class {
	#name : #InstanceVariableNotFound,
	#superclass : #Error,
	#instVars : [
		'instVarName'
	],
	#category : #Kernel-Exceptions
}

{ #category : #signalling }
InstanceVariableNotFound class >> signalFor: aString [
	"Create and signal a NoInstanceVariableNAmed exception for aString in the default receiver."
	
	^ self new
		instVarName: aString;
		signal
]

{ #category : #accessing }
InstanceVariableNotFound >> instVarName [
	^ instVarName
]

{ #category : #accessing }
InstanceVariableNotFound >> instVarName: anObject [
	instVarName := anObject
]

{ #category : #accessing }
InstanceVariableNotFound >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
InstanceVariableNotFound >> standardMessageText [
	"Generate a standard textual description"

	^ String
		streamContents: [ :stream | 
			stream nextPutAll: self instVarName.
			stream nextPutAll: ' not found in '.
			stream print: self signaler ]
]
