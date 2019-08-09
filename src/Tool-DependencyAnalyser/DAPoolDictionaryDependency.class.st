"
I am a dependency produced by a pool Dictionary.
For example the following class CharacterBlock depends on the Text package because it uses TextConstants that is defined in it.

Rectangle subclass: #CharacterBlock
	instanceVariableNames: 'stringIndex text textLine'
	classVariableNames: ''
	poolDictionaries: 'TextConstants'
	package: 'Text-Scanning-Base'
"
Class {
	#name : #DAPoolDictionaryDependency,
	#superclass : #DADependencyFromClass,
	#instVars : [
		'superclass',
		'poolDictionary'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #accessing }
DAPoolDictionaryDependency >> poolDictionary [
	^ poolDictionary
]

{ #category : #accessing }
DAPoolDictionaryDependency >> poolDictionary: aClass [
	poolDictionary := aClass
]

{ #category : #printing }
DAPoolDictionaryDependency >> printReasonOn: aStream [
	aStream
		nextPutAll: self theClass name;
		nextPutAll: ' uses pool dictionary ';
		nextPutAll: self poolDictionary name
]
