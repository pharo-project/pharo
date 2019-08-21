Class {
	#name : #UndeclaredDictionaryTest,
	#superclass : #WeakValueDictionaryTest,
	#category : #'System-Support-Tests'
}

{ #category : #running }
UndeclaredDictionaryTest >> classToBeTested [
	^ UndeclaredDictionary
]

{ #category : #tests }
UndeclaredDictionaryTest >> testAtPutNil [
	"self run: #testAtPutNil"

	| collection keyIn newValue |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.
	newValue := self valueNotIn.
	self assert: (collection at: nil put: newValue) == newValue.
	self supportsNilKey
		ifTrue: [ self assert: (collection at: nil) identicalTo: newValue ]
		ifFalse: [ self deny: (collection includesKey: nil) ].
	self assert: (collection at: keyIn put: nil) isNil.
	self deny: (collection includesKey: keyIn).
	self assert: (collection at: self keyNotIn put: nil) isNil.
	self deny: (collection includesKey: self keyNotIn).
	self assert: (collection at: nil put: nil) isNil.
	self deny: (collection includesKey: nil)
]

{ #category : #tests }
UndeclaredDictionaryTest >> testIncludesAssociationNoValue [
	| association dictionary |
	association := Association key: #key.
	
	self assert: association value isNil.

	dictionary := self collectionClass new.
	dictionary add: association.

	self deny: (dictionary includesKey: #key)
]

{ #category : #tests }
UndeclaredDictionaryTest >> testOccurrencesOf [
	| dict |
	dict := self collectionClass new.
	dict at: #a put: 1.
	dict at: #b put: 2.
	dict at: #c put: 1.
	dict at: #d put: 3.
	dict at: nil put: nil.
	dict at: #z put: nil.

	self assert: (dict occurrencesOf: 1) equals: 2.

	"When a class is GC we don't need to be able to count the occurences of the removed classes."
	self assert: (dict occurrencesOf: nil) equals: 0
]
