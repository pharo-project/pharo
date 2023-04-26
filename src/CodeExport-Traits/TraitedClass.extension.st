Extension { #name : #TraitedClass }

{ #category : #'*CodeExport-Traits' }
TraitedClass >> fileOutLocalMethodsInProtocolNamed: protocolName on: aFileStream [

	aFileStream cr.

	(self selectorsInProtocolNamed: protocolName)
		select: [ :selector | (self isLocalSelector: selector) or: [ (self traitComposition selectors includes: selector) not ] ]
		thenDo: [ :selector | self printMethodChunk: selector on: aFileStream ]
]
