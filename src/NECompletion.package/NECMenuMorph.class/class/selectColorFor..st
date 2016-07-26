preferences
selectColorFor: aSymbol 
	| attribute |
	attribute := self convertToSHSymbol: aSymbol.
	^ (SHTextStylerST80 new attributesFor: attribute) first color