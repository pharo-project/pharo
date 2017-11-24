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

{ #category : #requirements }
SmallDictionaryTest >> classToBeTested [

	^ SmallDictionary
]

{ #category : #'tests - printing' }
SmallDictionaryTest >> testStoreOn [
	"self debug: #testStoreOn"
	
	self assert: self nonEmptyDict storeString = ('((', self nonEmptyDict class printString , ' new) add: (#a->1); add: (#b->30); add: (#c->1); add: (#d-> -2); yourself)')
]
