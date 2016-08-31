*NautilusCommon
transformSelectorForComparison
	| selectorString selectorStringSize writeStream |
	"asString to convert Symbols"
	selectorString := self asString.
	selectorStringSize := self size.
	writeStream := (selectorString class new: selectorStringSize) writeStream.
	"to:do: for speed"
	1 to: selectorStringSize do: [ :i | 
		| nextChar |
		(nextChar := selectorString at: i) == $:
			ifFalse: [ writeStream nextPut: nextChar ] ].
	"faster to use asLowercase on the entire string after instead of on chars
	in the to:do: loop because of the bad Character>>#asLowercase implementation"
	writeStream position < selectorStringSize
		ifTrue: [ ^ writeStream contents asLowercase ]
		ifFalse: [ ^ selectorString asLowercase ]