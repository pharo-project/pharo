accessing
category: aCategory
	aCategory isEmpty ifTrue: [ InvalidPackageName signal: 'The Category should not be empty' ].
	category := aCategory