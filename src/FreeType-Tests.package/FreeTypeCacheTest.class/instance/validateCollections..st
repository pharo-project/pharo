private
validateCollections: aFreeTypeCache
	"check that the fifo list entries match the fontTable dict hierarchy"
	| fontTable fontTableEntries fifo lastLink |
	fontTable := aFreeTypeCache instVarNamed: #fontTable.
	fifo := aFreeTypeCache instVarNamed: #fifo.
	lastLink := (fifo instVarNamed:#lastLink).
	fontTableEntries := Set new.
	fontTable keysAndValuesDo:[:k1 :v1 |
		v1 keysAndValuesDo:[:k2 :v2 |
			v2 keysAndValuesDo:[:k3 :v3 |
				fontTableEntries add: v3 ]]].
	self assert: fifo size = fontTableEntries size.
	self assert: (fifo asSet = fontTableEntries).
	self assert: (lastLink isNil or:[lastLink nextLink isNil]) 
	

	