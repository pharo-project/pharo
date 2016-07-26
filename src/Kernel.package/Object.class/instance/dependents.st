dependencies
dependents
	"Answer a collection of objects that are 'dependent' on the receiver;
	 that is, all objects that should be notified if the receiver changes."

	^ self myDependents ifNil: [#()]