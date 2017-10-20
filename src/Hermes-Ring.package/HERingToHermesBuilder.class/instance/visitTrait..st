visiting
visitTrait: aTrait
	| aHETrait |
	
	aHETrait := HETrait new.
	self visitMethodContainer: aTrait using: aHETrait.

	aHETrait traitName: aTrait name.
	
	^ aHETrait.