Extension { #name : 'TraitedClass' }

{ #category : '*CodeExport' }
TraitedClass >> fileOutLocalMethodsInProtocol: protocol on: aFileStream [

	aFileStream cr.

	protocol methodSelectors
		select: [ :selector | (self isLocalSelector: selector) or: [ (self traitComposition selectors includes: selector) not ] ]
		thenDo: [ :selector | self printMethodChunk: selector on: aFileStream ]
]
