Extension { #name : #Integer }

{ #category : #'*Deprecated80' }
Integer >> digitAt: index [

	self
		deprecated: 'Please use byteAt: instead.'
		transformWith:
			'`@receiver digitAt: `@statements1'
				-> '`@receiver byteAt: `@statements1'.
	
	^ self byteAt: index
]

{ #category : #'*Deprecated80' }
Integer >> digitAt: index put: value [ 

	self
		deprecated: 'Please use #byteAt:put: instead'
		transformWith:
			'`@receiver digitAt: `@statements1 put: `@statements2'
		   -> '`@receiver byteAt: `@statements1 put: `@statements2'.

	^ self byteAt: index put: value 
]

{ #category : #'*Deprecated80' }
Integer >> digitCompare: arg [ 

	self
		deprecated: 'Please use bytesCompare: instead.'
		transformWith: '`@receiver digitCompare: `@statements1' 
						-> '`@receiver bytesCompare: `@statements1'.
	
	^ self bytesCompare: arg
]

{ #category : #'*Deprecated80' }
Integer >> digitLength [

	self
		deprecated: 'Use #bytesCount instead.'
		transformWith: '`@receiver digitLength' -> '`@receiver bytesCount'.

	^ self bytesCount
]
