accessing
next: anInteger putAll: aCollection startingAt: startIndex
	"Optimized for ByteArrays"
	aCollection class == ByteArray 
		ifTrue:[^super next: anInteger putAll: aCollection asString startingAt: startIndex].
	^super next: anInteger putAll: aCollection startingAt: startIndex