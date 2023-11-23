"
SUnit tests for weak key dictionaries
"
Class {
	#name : 'WeakKeyDictionaryTest',
	#superclass : 'DictionaryTest',
	#instVars : [
		'keys',
		'dict'
	],
	#category : 'Collections-Weak-Tests-Dictionaries',
	#package : 'Collections-Weak-Tests',
	#tag : 'Dictionaries'
}

{ #category : 'building suites' }
WeakKeyDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : 'coverage' }
WeakKeyDictionaryTest >> classToBeTested [

	^ WeakKeyDictionary
]

{ #category : 'running' }
WeakKeyDictionaryTest >> setUp [
	super setUp.
	keys := (1 to: 1000) collect:[:n| 'key', n asString].
	dict := WeakKeyDictionary new.
	keys do:[:n| dict at: n put: n,n].
	super setUp
]

{ #category : 'accessing' }
WeakKeyDictionaryTest >> size [
	"Answer the number of keys that should be used in testing."
	^1000
]

{ #category : 'tests' }
WeakKeyDictionaryTest >> testClearing [
	keys := nil.
	dict at: self put: nil.
	self assert: dict size equals: self size + 1.

	Smalltalk garbageCollect.

	"Keys are gone but not yet finalized."
	self assert: dict size equals: self size + 1.
	self assert: dict keys size equals: 1.

	"Now the size should be adjusted."
	self assert: dict size equals: 1
]

{ #category : 'tests' }
WeakKeyDictionaryTest >> testGrow [
	keys := nil.
	dict at: self put: nil.
	Smalltalk garbageCollect.

	"Keys are gone but not yet finalized."
	dict grow
]
