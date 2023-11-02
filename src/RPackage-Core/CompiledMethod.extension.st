Extension { #name : 'CompiledMethod' }

{ #category : '*RPackage-Core' }
CompiledMethod >> isDefinedInPackage: anRPackage [

	^ self origin package = anRPackage
]

{ #category : '*RPackage-Core' }
CompiledMethod >> isExtension [
	"I return true if a method is an extension method. Which means that the methods is not packaged in the package of the class containing the method, but in another package."

	"(self >> #selector) isExtension >>> false"

	"(self >> #traitSource) isExtension >>> true"

	^ self hasProperty: #extensionPackage
]

{ #category : '*RPackage-Core' }
CompiledMethod >> isExtensionInPackage: anRPackage [
	^ anRPackage includesExtensionSelector: self selector ofClass: self methodClass
]

{ #category : '*RPackage-Core' }
CompiledMethod >> removeFromPackage [

	self isFromTrait ifFalse: [ self package ifNotNil: [ :package | package removeMethod: self ] ]
]
