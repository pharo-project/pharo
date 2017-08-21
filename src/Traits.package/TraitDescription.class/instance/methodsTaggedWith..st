accessing tags
methodsTaggedWith: aSymbol
	"Any method could be tagged with multiple symbols for user purpose. 
	And class contains all method tags which used or not used yet by methods.
	This method select all methods marked by given tag.
	Tags are not inherited from traits. So we only select local methods"
	
	^self localMethods select: [ :each | each isTaggedWith: aSymbol ]