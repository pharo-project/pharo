Extension { #name : #TraitedMetaclass }

{ #category : #'*CodeExport-Traits' }
TraitedMetaclass >> fileOutLocalMethodsInCategory: aSymbol on: aFileStream [
	| selectors |
	
	aFileStream newLine.
	selectors := self selectorsToFileOutCategory: aSymbol.
	
	selectors do: [:sel | 
		(self isLocalSelector: sel) ifTrue: [ 
		self printMethodChunk: sel on: aFileStream ]].
	
	^ self
]
