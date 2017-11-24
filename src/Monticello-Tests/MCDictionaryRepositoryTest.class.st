Class {
	#name : #MCDictionaryRepositoryTest,
	#superclass : #MCRepositoryTest,
	#instVars : [
		'dict'
	],
	#category : #'Monticello-Tests'
}

{ #category : #actions }
MCDictionaryRepositoryTest >> addVersion: aVersion [
	dict at: aVersion info put: aVersion
]

{ #category : #utility }
MCDictionaryRepositoryTest >> deleteNode: aNode [
	dict removeKey: aNode
]

{ #category : #utility }
MCDictionaryRepositoryTest >> dictionary [
	^ dict ifNil: [dict := Dictionary new]
]

{ #category : #running }
MCDictionaryRepositoryTest >> setUp [
	repository :=  MCDictionaryRepository new dictionary: self dictionary
]
