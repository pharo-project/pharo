Extension { #name : #CompiledCode }

{ #category : #'*RPackage-Core' }
CompiledCode >> package [

	| originSelector |
	"This method returns the package this method belongs to.
	It takes into account classes and traits.
	If the method is in no package, returns nil by now"
	originSelector := self originMethod selector.

	^ self origin packages
		  detect: [ :each |
			  self origin isMeta
				  ifFalse: [ each includesSelector: originSelector ofClassName: self origin instanceSide originalName ]
				  ifTrue: [ each includesSelector: originSelector ofMetaclassName: self origin instanceSide originalName ] ]
		  ifNone: [ nil ]
]
