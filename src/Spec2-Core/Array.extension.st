Extension { #name : #Array }

{ #category : #'*Spec2-Core' }
Array >> asSpLayout [
	^ SpLegacyArrayLayout new
		array: self;
		yourself
]
