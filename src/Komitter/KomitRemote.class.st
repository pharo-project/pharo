Class {
	#name : #KomitRemote,
	#superclass : #Model,
	#instVars : [
		'remote'
	],
	#classInstVars : [
		'icon'
	],
	#category : #'Komitter-Models'
}

{ #category : #'*Komitter-UI' }
KomitRemote >> icon [

	^ nil
]

{ #category : #'*Komitter-UI' }
KomitRemote >> komitOn: aKomitterUI [

	^ aKomitterUI newKomitFor: self
]

{ #category : #'*Komitter-UI' }
KomitRemote >> label [

	^ ''
]

{ #category : #'as yet unclassified ' }
KomitRemote >> password [
	
	^ self remote password
]

{ #category : #protocol }
KomitRemote >> password: aString [

	self remote password: aString
]

{ #category : #accessing }
KomitRemote >> remote [
	^ remote
]

{ #category : #accessing }
KomitRemote >> remote: anObject [
	remote := anObject
]

{ #category : #protocol }
KomitRemote >> username [
	
	^ self remote user
]

{ #category : #protocol }
KomitRemote >> username: aString [ 
	
	self remote user: aString
]
