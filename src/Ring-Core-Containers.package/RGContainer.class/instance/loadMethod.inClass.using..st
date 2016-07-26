image class and method loading
loadMethod: aRGMethodDefinition inClass: aRGBehaviorDefinition using: packageKeys

	self addMethod: aRGMethodDefinition.
	aRGBehaviorDefinition addMethod: aRGMethodDefinition.
	self setPackageOfMethod: aRGMethodDefinition ofClass: aRGBehaviorDefinition using: packageKeys