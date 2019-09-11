Extension { #name : #SpLayoutFrame }

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutFrame >> asMorphicLayout [
	
	^ LayoutFrame identity
		topFraction: self topFraction;
		topOffset: self topOffset;
		bottomFraction: self bottomFraction;
		bottomOffset: self bottomOffset;
		rightFraction: self rightFraction;
		rightOffset: self rightOffset;
		leftFraction: self leftFraction;
		leftOffset: self leftOffset;
		yourself
]
