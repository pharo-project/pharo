Extension { #name : 'TraitedMetaclass' }

{ #category : '*CodeExport' }
TraitedMetaclass >> fileOutLocalMethodsInProtocol: protocol on: aFileStream [

	aFileStream cr.

	protocol methodSelectors
		select: [ :selector | self isLocalSelector: selector ]
		thenDo: [ :selector | self printMethodChunk: selector on: aFileStream ]
]
