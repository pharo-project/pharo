comparing
isCloserMatchThan: otherMember toStretch: inputStretch weight: inputWeight slant: inputSlant 
	| inputVector vector otherVector distance otherDistance dotProduct otherDotProduct |
	
	inputVector := self closenessVectorForStretch: inputStretch slant: inputSlant weight: inputWeight.
	vector := self closenessVector.
	otherVector := otherMember closenessVector.
	distance := (((inputVector first - vector first) raisedTo: 2) +
			((inputVector second - vector second) raisedTo: 2) +
			((inputVector third - vector third) raisedTo: 2)) sqrt.
	otherDistance := (((inputVector first - otherVector first) raisedTo: 2) +
			((inputVector second - otherVector second) raisedTo: 2) +
			((inputVector third - otherVector third) raisedTo: 2)) sqrt.
	distance < otherDistance ifTrue:[^true].
	distance > otherDistance ifTrue:[^false].	
	dotProduct := (inputVector first * vector first) +
				(inputVector second * vector second) +
				(inputVector third * vector third).
	otherDotProduct := (inputVector first * otherVector first) +
				(inputVector second * otherVector second) +
				(inputVector third * otherVector third).
	dotProduct > otherDotProduct ifTrue:[^true].
	dotProduct < otherDotProduct ifTrue:[^false].	
	vector first > otherVector first ifTrue:[^true].
	vector first < otherVector first ifTrue:[^false].
	vector second > otherVector second ifTrue:[^true].
	vector second < otherVector second ifTrue:[^false].
	vector third > otherVector third ifTrue:[^true].
	vector third < otherVector third ifTrue:[^false].
	^false 