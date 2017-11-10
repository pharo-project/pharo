Class {
	#name : #MetacelloProjectRegistrationVersionInfo,
	#superclass : #Object,
	#instVars : [
		'versionString',
		'projectVersion'
	],
	#category : #Metacello-Core-Scripts
}

{ #category : #accessing }
MetacelloProjectRegistrationVersionInfo >> projectVersion [
  ^ projectVersion
]

{ #category : #accessing }
MetacelloProjectRegistrationVersionInfo >> projectVersion: anObject [
  projectVersion := anObject
]

{ #category : #accessing }
MetacelloProjectRegistrationVersionInfo >> setVersionString: anObject [
  "set versionString, only if it does not already have avalue"

  versionString ifNil: [ self versionString: anObject ]
]

{ #category : #accessing }
MetacelloProjectRegistrationVersionInfo >> versionString [

   ^versionString
]

{ #category : #accessing }
MetacelloProjectRegistrationVersionInfo >> versionString: anObject [
  versionString := anObject.
  projectVersion := nil
]
