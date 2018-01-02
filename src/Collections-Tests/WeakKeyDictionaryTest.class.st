"
SUnit tests for weak key dictionaries
"
Class {
	#name : #WeakKeyDictionaryTest,
	#superclass : #DictionaryTest,
	#instVars : [
		'keys',
		'dict'
	],
	#category : #'Collections-Tests-Weak'
}

{ #category : #'building suites' }
WeakKeyDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #examples }
WeakKeyDictionaryTest class >> timingLarge [
	| dict keys size loops |
	
	size := 100000.
	loops := 10.
	
	keys := (1 to: size) collect:[:n| 'foo',n asString].
	Smalltalk garbageCollect.
	dict := WeakKeyDictionary new.
	keys do:[:k| dict at: k put: k].
	Smalltalk garbageCollect.
	^Time millisecondsToRun: [
		loops timesRepeat:[
			dict at: 'bar','0' put: 100.
			Smalltalk garbageCollectMost.
			dict finalizeValues.
		]].

]

{ #category : #examples }
WeakKeyDictionaryTest class >> timingMany [
	| dicts keys size loops count |
	
	size := 10.
	count := 10000.
	loops := 10.
	
	keys := (1 to: size) collect:[:n| 'foo',n asString].
	
	dicts := OrderedCollection new.
	count timesRepeat:[ | dict |
		dict := WeakKeyDictionary new.
		keys do:[:k| dict at: k put: k].
		dicts add: dict.
	].
	Smalltalk garbageCollect.
	^Time millisecondsToRun: [
			loops timesRepeat:[
				dicts do: [:d| d finalizeValues].
			]].

]

{ #category : #requirements }
WeakKeyDictionaryTest >> classToBeTested [
	^ WeakKeyDictionary
]

{ #category : #running }
WeakKeyDictionaryTest >> setUp [
	super setUp.
	keys := (1 to: 1000) collect:[:n| 'key', n asString].
	dict := WeakKeyDictionary new.
	keys do:[:n| dict at: n put: n,n].
	super setUp.
]

{ #category : #accessing }
WeakKeyDictionaryTest >> size [
	"Answer the number of keys that should be used in testing."
	^1000
]

{ #category : #requirements }
WeakKeyDictionaryTest >> supportsNilKey [
	^ false
]

{ #category : #tests }
WeakKeyDictionaryTest >> testClearing [
	keys := nil.
	dict at: self put: nil.
	self assert: dict size = (self size + 1).

	Smalltalk garbageCollect.

	"Keys are gone but not yet finalized."
	self assert: dict size = (self size + 1).
	self assert: dict keys size = 1.
	
	dict finalizeValues.
	
	"Now the size should be adjusted."
	self assert: dict size = 1.
	
]

{ #category : #tests }
WeakKeyDictionaryTest >> testGrow [
	keys := nil.
	dict at: self put: nil.
	Smalltalk garbageCollect.

	"Keys are gone but not yet finalized."
	dict grow.
]

{ #category : #'tests - printing' }
WeakKeyDictionaryTest >> testStoreOnWithNegativeInteger [
	| dictionary |
	dictionary := { 'x' -> -1 } as: self classToBeTested.
	
	self
		assert: (String streamContents: [ :s | dictionary storeOn: s ])
		equals: '((',self classToBeTested name,' new) add: (WeakKeyAssociation key: ''x'' value: -1); yourself)'
]
