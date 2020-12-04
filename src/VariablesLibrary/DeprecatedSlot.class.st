"
Description
--------------------

I am a slot used when the usage of an instance variable should be deprecated.

Examples
--------------------

	FamixMetamodelGenerator subclass: #FamixBasicInfrastructureGenerator
	slots: { #entity => DeprecatedSlot message: 'Do not use'. 
				#sourceAnchor. #sourceLanguage. #sourcedEntity. #comment. #namedEntity. #sourceTextAnchor. #unknownSourceLanguage }
	classVariables: {  }
	package: 'Famix-BasicInfrastructure'
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	message:		<aString>		The deprecation message to show.

"
Class {
	#name : #DeprecatedSlot,
	#superclass : #IndexedSlot,
	#instVars : [
		'message'
	],
	#category : #'VariablesLibrary-DeprecatedSlot'
}

{ #category : #accessing }
DeprecatedSlot class >> message: aString [
	^ self new
		message: aString;
		yourself
]

{ #category : #accessing }
DeprecatedSlot >> message [
	^ message
]

{ #category : #accessing }
DeprecatedSlot >> message: anObject [
	message := anObject
]

{ #category : #printing }
DeprecatedSlot >> printOn: aStream [
	aStream
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name.
	aStream
		nextPutAll: ' message: ';
		store: self message
]

{ #category : #'meta-object-protocol' }
DeprecatedSlot >> read: anObject [
	SlotDeprecation new
		context: thisContext;
		explanation: self message;
		signal.
	super read: anObject
]

{ #category : #'meta-object-protocol' }
DeprecatedSlot >> write: aValue to: anObject [
	SlotDeprecation new
		context: thisContext;
		explanation: self message;
		signal.
	super write: aValue to: anObject
]
