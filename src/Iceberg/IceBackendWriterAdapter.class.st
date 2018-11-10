"
Adapter for having MCFileTreeWriters being able to write into Iceberg backend repositories without introducing unpleasant modifications in them.
"
Class {
	#name : #IceBackendWriterAdapter,
	#superclass : #Object,
	#instVars : [
		'backend'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #'instance creation' }
IceBackendWriterAdapter class >> for: anIceBackend [
	^ self new backend: anIceBackend; yourself.
]

{ #category : #accessing }
IceBackendWriterAdapter >> backend [
	^ backend
]

{ #category : #accessing }
IceBackendWriterAdapter >> backend: anObject [
	backend := anObject
]

{ #category : #accesing }
IceBackendWriterAdapter >> directory [
	^ self backend codeDirectory 
]

{ #category : #acessing }
IceBackendWriterAdapter >> propertyFileExtension [
	^ MCFileTreeRepository defaultPropertyFileExtension
]
