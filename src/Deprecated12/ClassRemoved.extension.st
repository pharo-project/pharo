Extension { #name : 'ClassRemoved' }

{ #category : '*Deprecated12' }
ClassRemoved >> categoryName [

	self deprecated: 'Use package and tag instead of the category.'.
	^ self packageTagAffected categoryName
]

{ #category : '*Deprecated12' }
ClassRemoved >> packageTag [

	self deprecated: 'Use #packageTagAffected instead.' transformWith: '`@rcv packageTag' -> '`@rcv packageTagAffected'.
	^ self packageTagAffected
]
