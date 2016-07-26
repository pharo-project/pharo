as yet unclassified
allVersionDescriptors
	^ self repository allVersionDescriptors select: [ :each | each commitID hex = self commitId ]
