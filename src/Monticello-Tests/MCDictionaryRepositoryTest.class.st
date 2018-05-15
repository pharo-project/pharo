Class {
	#name : #MCDictionaryRepositoryTest,
	#superclass : #MCRepositoryTest,
	#instVars : [
		'dict'
	],
	#category : #'Monticello-Tests-Repository'
}

{ #category : #actions }
MCDictionaryRepositoryTest >> addVersion: aVersion [
	dict at: aVersion info put: aVersion
]

{ #category : #utilities }
MCDictionaryRepositoryTest >> deleteNode: aNode [
	dict removeKey: aNode
]

{ #category : #utilities }
MCDictionaryRepositoryTest >> dictionary [
	^ dict ifNil: [dict := Dictionary new]
]

{ #category : #running }
MCDictionaryRepositoryTest >> setUp [
	super setUp.
	repository :=  MCDictionaryRepository new dictionary: self dictionary
]
