Extension { #name : 'CompiledMethod' }

{ #category : '*RPackage-Core' }
CompiledMethod >> isExtension [
	"I return true if a method is an extension method. Which means that the methods is not packaged in the package of the class containing the method, but in another package."

	"(self >> #selector) isExtension >>> false"

	"(self >> #traitSource) isExtension >>> true"

	^ self hasProperty: #extensionPackage
]

{ #category : '*RPackage-Core' }
CompiledMethod >> isExtensionInPackage: aPackage [

	self
		deprecated: 'This method will be removed from future version of Pharo because it is too specific (I doubt anybody uses this) and easy to inline.'
		transformWith: '`@rcv isExtensionInPackage: `@package' -> '`@rcv isExtension and: [ `@rcv package = `@package ]'.
	^ self isExtension and: [ self package = aPackage ]
]

{ #category : '*RPackage-Core' }
CompiledMethod >> removeFromPackage [

	self isFromTrait ifFalse: [ self package removeMethod: self ]
]
