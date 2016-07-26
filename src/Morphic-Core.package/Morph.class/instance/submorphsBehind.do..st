submorphs-accessing
submorphsBehind: aMorph do: aBlock
	| behind |
	behind := false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind := true]
						ifFalse: [behind ifTrue: [aBlock value: m]]].
