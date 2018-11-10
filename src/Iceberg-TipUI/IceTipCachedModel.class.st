"
I'm a cached model. 
I take all unary messages sent to my realObject and I cache their values. 
I keep a cache with rules for reset it, which can be accessedon the #resetOn: family of messages.
"
Class {
	#name : #IceTipCachedModel,
	#superclass : #Object,
	#instVars : [
		'realObject',
		'cache'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #'instance creation' }
IceTipCachedModel class >> for: anObject [
	^ self basicNew initializeObject: anObject
]

{ #category : #'reflective operations' }
IceTipCachedModel >> doesNotUnderstand: aMessage [
	aMessage selector isUnary
		ifTrue: [ 
			cache 
				at: aMessage selector
				ifPresent: [ :value | ^ value ].
			 (self realObject class lookupSelector: aMessage selector)
				ifNotNil: [ :method |
					(method hasPragmaNamed: #noCache)
						ifFalse: [ 
							^ cache
								at: aMessage selector 
								put: (self forwardMessage: aMessage) ] ] ].
	^ self forwardMessage: aMessage
]

{ #category : #'reflective operations' }
IceTipCachedModel >> forwardMessage: aMessage [
	^ realObject 
		perform: aMessage selector 
		withArguments: aMessage arguments 
]

{ #category : #initialization }
IceTipCachedModel >> initializeObject: anObject [
	realObject := anObject.
	cache := IceTipCache proxy: self.
	self initialize
]

{ #category : #accessing }
IceTipCachedModel >> name [
	self flag: #pharo6. "in pharo6 there is still Object>>#name so this request will 
	not be redirected as expected (so we'll do it explicitly)"
	^ self realObject name
]

{ #category : #printing }
IceTipCachedModel >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	self realObject printOn: aStream.
	aStream nextPut: $)
]

{ #category : #accessing }
IceTipCachedModel >> realObject [
	^ realObject
]

{ #category : #accessing }
IceTipCachedModel >> reset [
	cache reset
]
