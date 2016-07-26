instance creation
text: aText icon: form
	| morph |
	
	aText ifNotNil: [ morph := aText asMorph 
		lock;
		yourself ].

	^ self new
		morph:	morph;
		icon: form;
		yourself