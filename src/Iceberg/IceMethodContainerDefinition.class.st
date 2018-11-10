"
I am an abstract class representing an IceDefinition that is a container of methods. I.e., a class, a trait or an extension.

I have a name which is the name of the container I represent. If I'm meta-side, I add to my name a suffix class. For example: Integer class.

I also contain the monticello definition that corresponds to me.

See my super/subclasses for more information.
"
Class {
	#name : #IceMethodContainerDefinition,
	#superclass : #IceCodeDefinition,
	#instVars : [
		'isMeta',
		'mcDefinition'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceMethodContainerDefinition class >> named: aString isMeta: isMeta [
	
	^ self new
		name: aString;
		isMeta: isMeta;
		yourself
]

{ #category : #patching }
IceMethodContainerDefinition >> asMCDefinition [

	^ mcDefinition
]

{ #category : #accessing }
IceMethodContainerDefinition >> contents [
	
	mcDefinition ifNil: [ ^ '' ].
	^ self isMeta
		ifFalse: [ mcDefinition definitionString ]
		ifTrue: [ mcDefinition classDefinitionString ]
]

{ #category : #accessing }
IceMethodContainerDefinition >> isMeta [
	^ isMeta
]

{ #category : #accessing }
IceMethodContainerDefinition >> isMeta: anObject [
	isMeta := anObject
]

{ #category : #accessing }
IceMethodContainerDefinition >> key [
	
	^ super key , (self isMeta ifTrue: [ ' class' ] ifFalse: [ '' ])
]

{ #category : #patching }
IceMethodContainerDefinition >> mcDefinition: aMCDefinition [

	mcDefinition := aMCDefinition
]

{ #category : #accessing }
IceMethodContainerDefinition >> name [
	^ name , (isMeta ifTrue: [ ' class' ] ifFalse: [ '' ])
]

{ #category : #accessing }
IceMethodContainerDefinition >> name: aString [

	name := aString
]

{ #category : #patching }
IceMethodContainerDefinition >> removeFrom: aPatcher [
	
	aPatcher removeClass: self
]
