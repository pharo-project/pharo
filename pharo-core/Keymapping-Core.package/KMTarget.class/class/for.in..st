instance creation
for: aTarget in: aMorph
	^ (self new)
		targetSelector: aTarget;
		morph: aMorph;
		yourself