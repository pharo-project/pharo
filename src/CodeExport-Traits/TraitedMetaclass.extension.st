Extension { #name : #TraitedMetaclass }

{ #category : #'*CodeExport-Traits' }
TraitedMetaclass >> fileOutLocalMethodsInProtocolNamed: protocolName on: aFileStream [

	aFileStream cr.

	(self selectorsInProtocolNamed: protocolName)
		select: [ :selector | self isLocalSelector: selector ]
		thenDo: [ :selector | self printMethodChunk: selector on: aFileStream ]
]
