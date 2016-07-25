private
noCheckAddForRehash: anAssociation
	| cleanedValue |
	anAssociation key ifNil:[^self].
	cleanedValue := anAssociation value copyWithout: nil.
	cleanedValue notEmpty ifTrue:[
		anAssociation value: cleanedValue.
		super noCheckAddForRehash: anAssociation.
	].