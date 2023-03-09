Extension { #name : #TraitedMetaclass }

{ #category : #'*CodeExport-Traits' }
TraitedMetaclass >> fileOutLocalMethodsInCategory: aProtocol on: aFileStream [

	| selectors |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aProtocol.

	selectors do: [ :sel | (self isLocalSelector: sel) ifTrue: [ self printMethodChunk: sel on: aFileStream ] ].

	^ self
]
