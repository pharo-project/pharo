*System-Support
referencedClasses
	"Return the set of classes that are directly referenced by my methods"

	^ self methods flatCollectAsSet: #referencedClasses