finalization
expiredValuesDo: aBlock
	"Clear all associations with nil keys"
	1 to: array size do:[:i | | assoc |
		assoc := array at: i.
		(assoc notNil and: [ assoc key isNil and: [assoc expired not ]])
		ifTrue: [ 
			aBlock value: assoc value.
			assoc expire.
			expired := expired + 1.
			tally := tally - 1.	]]