private
validateSizes: aFreeTypeCache
	"check that the used, maximumSize, and caches entries are valid"
	| fontTable calcSize max used |
	fontTable := aFreeTypeCache instVarNamed: #fontTable.
	used := aFreeTypeCache instVarNamed: #used.
	max := aFreeTypeCache instVarNamed: #maximumSize.
	calcSize := 0.
	fontTable do:[:charCodeTable |
		charCodeTable do:[:typeTable |
			typeTable do:[:entry |
				calcSize := calcSize + (aFreeTypeCache sizeOf: entry object)]]].
	self assert: calcSize = used.
	self assert: (max isNil or:[used <= max])
	