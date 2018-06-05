Extension { #name : #SmalltalkImage }

{ #category : #'*Deprecated70' }
SmalltalkImage >> associationAt: key ifAbsent: aBlock [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver associationAt: `@statements1 ifAbsent: `@statements2'
		   -> '`@receiver globals associationAt: `@statements1 ifAbsent: `@statements2'.

	^globals associationAt: key ifAbsent: aBlock
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> do: aBlock [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver do: `@statements1'
		   -> '`@receiver globals do: `@statements1'.

	^globals do: aBlock
	

]

{ #category : #'*Deprecated70' }
SmalltalkImage >> includes: element [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver includes: `@statements1'
		   -> '`@receiver globals includes: `@statements1'.

	^globals includes: element
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> keyAtIdentityValue: anObject ifAbsent: aBlock [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver keyAtIdentityValue: `@statements1 ifAbsent: `@statements2'
		   -> '`@receiver globals keyAtIdentityValue: `@statements1 ifAbsent: `@statements2'.

	^globals keyAtIdentityValue: anObject ifAbsent: aBlock
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> keyAtValue: anObject [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver keyAtValue: `@statements1'
		   -> '`@receiver globals keyAtValue: keyAtIdentityValue: `@statements1'.

	^globals keyAtValue: anObject
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> keys [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver keys'
		   -> '`@receiver globals keys'.

	^globals keys
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> keysAndValuesDo: aBlock [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver keysAndValuesDo: `@statements1'
		   -> '`@receiver globals keysAndValuesDo: `@statements1'.

	^globals keysAndValuesDo: aBlock

]

{ #category : #'*Deprecated70' }
SmalltalkImage >> removeKey: key [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver removeKey: `@statements1'
		   -> '`@receiver globals removeKey: `@statements1'.

	^globals removeKey: key
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> removeKey: key ifAbsent: aBlock [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver removeKey: `@statements1 ifAbsent: `@statements2'
		   -> '`@receiver globals removeKey: `@statements1 ifAbsent: `@statements2'.

	^globals removeKey: key ifAbsent: aBlock
]

{ #category : #'*Deprecated70' }
SmalltalkImage >> select: aBlock [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver select: `@statements1'
		   -> '`@receiver globals select: `@statements1'.

	^globals select: aBlock

]

{ #category : #'*Deprecated70' }
SmalltalkImage >> size [

	self
		deprecated: 'Please use Smalltalk globals instead'
		transformWith:
			'`@receiver size'
		   -> '`@receiver globals size'.

	^globals size
]
