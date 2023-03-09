Extension { #name : #Categorizer }

{ #category : #'*CodeImport' }
Categorizer >> changeFromString: aString [
	"Parse the argument, aString, and make this be the receiver's structure."

	| protocolSpecs |
	protocolSpecs := aString parseLiterals.
	"If nothing was scanned and I had no elements before, then default me"
	(protocolSpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self setDefaultList: Array new].

	^ self changeFromCategorySpecs: protocolSpecs
]
