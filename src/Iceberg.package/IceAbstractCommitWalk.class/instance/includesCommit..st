walking
includesCommit: aCommit
	self commitsDo: [ :commit | 
		commit id = aCommit id ifTrue: [ ^ true ]].
	^ false