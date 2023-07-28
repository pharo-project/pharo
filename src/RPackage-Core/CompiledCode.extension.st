Extension { #name : #CompiledCode }

{ #category : #'*RPackage-Core' }
CompiledCode >> extensionPackage [
	"Private method - Do not use it if you don't know what to do.
	In case I am in an extension, I return the package containing me. Else return nil."

	| originSelector class |
	originSelector := self originMethod selector.
	class := self origin.

	^ class extendingPackages
		  detect: [ :extendedPackage | extendedPackage includesExtensionSelector: originSelector ofClass: class ]
		  ifNone: [ nil ]
]

{ #category : #'*RPackage-Core' }
CompiledCode >> package [
	"This method returns the package this method belongs to. It takes into account classes and traits.
	If the method is in no package, returns nil"

	self isInstalled ifFalse: [ ^ nil ].
	^ self extensionPackage ifNil: [ self origin package ]
]
