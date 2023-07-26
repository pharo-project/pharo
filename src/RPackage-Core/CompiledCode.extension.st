Extension { #name : #CompiledCode }

{ #category : #'*RPackage-Core' }
CompiledCode >> package [
	"This method returns the package this method belongs to. It takes into account classes and traits.
	If the method is in no package, returns nil"

	| originSelector class |
	originSelector := self originMethod selector.
	class := self origin.
 
	^ class extendingPackages
		detect: [ :extendedPackage |
			class isMeta
				ifFalse: [ extendedPackage includesExtensionSelector: originSelector ofClassName: class instanceSide originalName ]
				ifTrue: [ extendedPackage includesExtensionSelector: originSelector ofMetaclassName: class instanceSide originalName ] ]
		ifNone: [ class package ]
]
