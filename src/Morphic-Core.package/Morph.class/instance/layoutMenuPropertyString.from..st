layout-menu
layoutMenuPropertyString: aSymbol from: currentSetting 
	| wording |
	wording := String
				streamContents: [:stream | 
					| index | 
					index := 1.
					aSymbol keysAndValuesDo: [:idx :ch | ch isUppercase
								ifTrue: [stream nextPutAll: (aSymbol copyFrom: index to: idx - 1) asLowercase.
										stream nextPutAll: ' '.
										index := idx]].
					index < aSymbol size
						ifTrue: [stream nextPutAll: (aSymbol copyFrom: index to: aSymbol size) asLowercase]].
	^ (aSymbol == currentSetting) -> wording translated