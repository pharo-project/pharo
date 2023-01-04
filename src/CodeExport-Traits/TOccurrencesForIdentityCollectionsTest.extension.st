Extension { #name : #TOccurrencesForIdentityCollectionsTest }

{ #category : #'*CodeExport-Traits' }
TOccurrencesForIdentityCollectionsTest classSide >> fileOutLocalMethodsInCategory: aSymbol on: aFileStream [
	| selectors |

	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.

	selectors do: [:sel |
		((self isLocalSelector: sel) or: [ (self traitComposition selectors includes: sel) not]) ifTrue: [
		self printMethodChunk: sel on: aFileStream ]].

	^ self
]
