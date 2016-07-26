finalization
finalizeValues: finiObjects
	"Clear all associations with key == nil and value is in finiObjects."
	array do:[:assoc|
		(assoc notNil and:[finiObjects includes: assoc value]) ifTrue:[
			assoc expire.
			expired := expired + 1.
			tally := tally - 1.
		].
	].
