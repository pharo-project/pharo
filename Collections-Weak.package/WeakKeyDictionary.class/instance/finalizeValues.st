finalization
finalizeValues
	"Clear all associations with nil keys.
	Avoid using block closures, so it won't produce many garbage 
	(since pushing closure as argument requires creating its preinitialized copy)"
	1 to: array size do:[:i | | assoc |
		assoc := array at: i.
		assoc ifNotNil:[
			assoc expire ifTrue:[
				expired := expired + 1.
				tally := tally - 1.
			].
		].
	].
