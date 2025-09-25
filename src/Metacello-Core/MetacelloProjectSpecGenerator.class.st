Class {
	#name : 'MetacelloProjectSpecGenerator',
	#superclass : 'MetacelloAbstractSpecGenerator',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> projectSpecCreationBlock [
    ^ [ :projectName | {(MetacelloProject new projectSpec name: projectName)} ]
]

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> projectSpecListBlock [
    ^ [ MetacelloProjectRegistration projectSpecs ]
]

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> projectSpecLookupBlock [
