Extension { #name : #Rectangle }

{ #category : #'*Spec2-Layout' }
Rectangle >> asSpLayoutFrame [
	"Answer an instance of LayoutFrame which uses fractions from receiver "
	
	^ SpLayoutFrame new
		leftFraction: self left;
		rightFraction: self right;
		topFraction: self top;
		bottomFraction: self bottom;
		yourself
		
]
