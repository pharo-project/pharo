"
SUnit tests for small dictionaries
"
Class {
	#name : #SmallDictionaryTest,
	#superclass : #DictionaryTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #'building suites' }
SmallDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #coverage }
SmallDictionaryTest >> classToBeTested [

	^ SmallDictionary
]

{ #category : #'tests - new' }
SmallDictionaryTest >> testNewFromKeysAndValues2 [
 
	| dict newDict |
	dict := self classToBeTested new
		at: #a put: 1;
		at: #b put: 2;
		at: #c put: 3; yourself.
	newDict := self classToBeTested newFromKeys: dict keys andValues: dict values.
	dict keysAndValuesDo: [:k :v|
		self assert: (newDict at: k) equals: v ]. 
]

{ #category : #'tests - printing' }
SmallDictionaryTest >> testStoreOn [
	self
		assert: self nonEmptyDict storeString
		equals: '((' , self nonEmptyDict class printString , ' new) add: (#a->1); add: (#b->30); add: (#c->1); add: (#d-> -2); yourself)'
]
