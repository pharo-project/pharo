walking
commits
	| commits |
	commits := OrderedCollection new.
	self commitsDo: [ :commit | commits add: commit ].
	^ commits