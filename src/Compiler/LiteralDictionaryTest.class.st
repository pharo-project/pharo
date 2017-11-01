"
SUnit tests for literal dictionaries
"
Class {
	#name : #LiteralDictionaryTest,
	#superclass : #DictionaryTest,
	#category : 'Compiler-Tests'
}

{ #category : #'building suites' }
LiteralDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #requirements }
LiteralDictionaryTest >> classToBeTested [

^ LiteralDictionary
]

{ #category : #'tests - literal specific behavior' }
LiteralDictionaryTest >> testIncludesWithEqualElementFromDifferentClasses [

| dict |
dict := self classToBeTested new.

dict at: 1 put: 'element1'.
dict at: #key put: 1.0.

self deny: (dict includesKey: 1.0).
self assert: (dict includes: 1)
]

{ #category : #problems }
LiteralDictionaryTest >> testUnCategorizedMethods [
"this test doesn't pass :"
	| categories slips  |
	categories := self categoriesForClass: self targetClass.
	slips := categories select: [:each | each = #'as yet unclassified'].
	self should: [slips isEmpty].	
]
