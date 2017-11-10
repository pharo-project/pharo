"
I am ZnMultiValueDictionary. 
I am a Dictionary.

I offer an #at:add: method to transparently create Array valued multi entries when needed.
My #keysAndValuesDo: is overwritten to transparently deal with multi entries.
To merge two instance preserving multiple values you can use #addAllMulti:
I normally limit the number of entries to protect me from resource abuse.

Note that most other methods will show the actual value.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnMultiValueDictionary,
	#superclass : #Dictionary,
	#instVars : [
		'limit'
	],
	#category : #Zinc-Resource-Meta-Core
}

{ #category : #accessing }
ZnMultiValueDictionary class >> defaultLimit [
	"Return the maximum number dictionary entries to accept.
	Used by ZnMultiValueDictionary and thus for reading headers, 
	url query and application form url encoded entity fields.
	This helps to protect us from malicious content."
	
	^ ZnMaximumNumberOfDictionaryEntries value
]

{ #category : #adding }
ZnMultiValueDictionary >> add: anAssociation [
	self checkLimitForKey: anAssociation key.
	^ super add: anAssociation
]

{ #category : #adding }
ZnMultiValueDictionary >> addAllMulti: keyedCollection [
	"Note that we use #at:add:"

	keyedCollection keysAndValuesDo: [ :key :value | 
		self at: key add: value ]
]

{ #category : #accessing }
ZnMultiValueDictionary >> at: key add: value [
	"Store value under key. If key already exists, store multiple values as Array"
	
	| existingValue newValue |
	existingValue := self at: key ifAbsent: [ nil ].
	newValue := existingValue isNil
		ifTrue: [ value ] 
		ifFalse: [
			existingValue isArray 
				ifTrue: [ existingValue copyWith: value ]
				ifFalse: [ Array with: existingValue with: value ] ].
	^ self at: key put: newValue 


]

{ #category : #accessing }
ZnMultiValueDictionary >> at: key put: anObject [
	self checkLimitForKey: key.
	^ super at: key put: anObject
]

{ #category : #private }
ZnMultiValueDictionary >> checkLimitForKey: aKey [
	"Signal an exception when the limit, if present, is exceeded."
	
	(self limit notNil and: [ self size >= self limit and: [ (self includesKey: aKey) not ]])
		ifTrue: [ (ZnTooManyDictionaryEntries limit: self limit) signal ].

]

{ #category : #initialize-release }
ZnMultiValueDictionary >> initialize: n [
	super initialize: n.
	limit := self class defaultLimit
]

{ #category : #enumerating }
ZnMultiValueDictionary >> keysAndValuesDo: block [
	"Execute a two argument (key, value) block for each header.
	Multi-valued headers are handled transparently."

	super keysAndValuesDo: [ :key :value |
		value isArray
			ifTrue: [ 
				value do: [ :each | 
					block value: key value: each ] ]
			ifFalse: [
				block value: key value: value ] ]
]

{ #category : #accessing }
ZnMultiValueDictionary >> limit [
	^ limit
]

{ #category : #initialize-release }
ZnMultiValueDictionary >> limit: numberOfEntries [
	limit := numberOfEntries 
]

{ #category : #initialize-release }
ZnMultiValueDictionary >> unlimited [
	self limit: nil
]
