Extension { #name : 'MCFileBasedRepository' }

{ #category : '*MonticelloFileTree-Core' }
MCFileBasedRepository >> retrieveVersionsWithPackageNames: packageNames [
	| packageDescriptions |
	packageDescriptions := self packageDescriptionsFromReadableFileNames.
	packageNames addAll: (packageDescriptions collect: [ :packageDescription | packageDescription first ]).
	^ packageDescriptions select: [ :each | (each at: 3) isNumber ]
]
