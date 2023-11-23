Extension { #name : 'ClassAdded' }

{ #category : '*Deprecated12' }
ClassAdded >> classCategory [

	self deprecated: 'We should not rely on class categories anymore but asking the class for its #package or #packageTag instead.'.
	^ self classAffected category
]
