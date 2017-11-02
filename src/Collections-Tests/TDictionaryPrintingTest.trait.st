"
A trait for test purposes
"
Trait {
	#name : #TDictionaryPrintingTest,
	#category : 'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryPrintingTest >> emptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryPrintingTest >> newEmptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryPrintingTest >> nonEmptyDict [
	self explicitRequirement
]

{ #category : #'tests - printing' }
TDictionaryPrintingTest >> testPrintElementsOn [
	| str |
	str := String new writeStream.
	self nonEmptyDict printElementsOn: str.
	self assert: (str contents = '(#a->1 #b->30 #c->1 #d->-2 )')
]

{ #category : #'tests - printing' }
TDictionaryPrintingTest >> testStoreOn [
	| str |
	str := String new writeStream.
	self nonEmptyDict storeOn: str.
	self assert: str contents = '((Dictionary new) add: (#b->30); add: (#c->1); add: (#d->-2); add: (#a->1); yourself)'
]
