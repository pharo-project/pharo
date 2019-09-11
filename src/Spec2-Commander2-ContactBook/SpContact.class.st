"
I model a contact, I have a name and a phone.
"
Class {
	#name : #SpContact,
	#superclass : #Object,
	#instVars : [
		'name',
		'phone'
	],
	#category : #'Spec2-Commander2-ContactBook-Model'
}

{ #category : #'instance creation' }
SpContact class >> named: aString phone: phone [
	^self new 
		name: aString;
		phone: phone
]

{ #category : #accessing }
SpContact >> name [
	^ name
]

{ #category : #accessing }
SpContact >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SpContact >> phone [
	^ phone
]

{ #category : #accessing }
SpContact >> phone: anObject [
	phone := anObject
]

{ #category : #printing }
SpContact >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	aStream nextPutAll: name.
	aStream nextPut: $).
]
