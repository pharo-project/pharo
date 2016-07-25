halo handling
obtainHalo: aHalo
	"Used for transfering halos between hands"

	self halo == aHalo
		ifTrue: [ ^ self ].	
	"Find former owner"
	self world hands detect: [ :hand | hand halo == aHalo ] ifFound: [ :formerOwner | formerOwner releaseHalo: aHalo ].
	self halo: aHalo