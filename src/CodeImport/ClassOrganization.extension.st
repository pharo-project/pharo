Extension { #name : #ClassOrganization }

{ #category : #'*CodeImport' }
ClassOrganization >> changeFromString: aString [
	"Parse the argument, aString, and make this be the receiver's structure."

	| protocolSpecs |
	protocolSpecs := aString parseLiterals.

	self reset.

	"If nothing was scanned and I had no elements before, then default me"
	(protocolSpecs isEmpty and: [ self protocols isEmpty ]) ifTrue: [ ^ self ].

	protocolSpecs do: [ :spec |
		| name methods |
		name := spec first asSymbol.
		methods := spec allButFirst asSet.
		self addProtocol: (Protocol name: name methodSelectors: methods) ]
]
