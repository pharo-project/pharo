submorphs-accessing
submorphsInFrontOf: aMorph do: aBlock
	| behind |
	behind := false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind := true]
						ifFalse: [behind ifFalse: [aBlock value: m]]].
