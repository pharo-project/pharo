Extension { #name : 'Metacello' }

{ #category : '*Metacello-MVS' }
Metacello >> mvs [
	"Resolve with minimal version selection: semantic versions are minimums, the highest minimum of each repository wins. Metacello then records and loads the versions selected"

	self resolver: (MetacelloMVSResolver on: (MetacelloMVSGraph new source: MetacelloMVSVersionSource default))
]
