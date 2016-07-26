instance creation
target: aTargetSelector morph: aMorph
	^ (self new)
		target: (KMTarget for: aTargetSelector in: aMorph);
		yourself