installing
postloadOver: aDefinition
	super postloadOver: aDefinition.
	self class initializersEnabled ifTrue: [
		(self isInitializer
			and: [ self actualClass isTrait not 
					and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]]) ifTrue: [
				self actualClass theNonMetaClass initialize ] ].
	"Postloading of FFI fields. This code will be called when loading FFI structures that are not by default in the image. This is NOT dead code."
	self isExternalStructureFieldDefinition
		ifTrue: [self actualClass theNonMetaClass compileFields].