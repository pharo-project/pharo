converting
capitalized
	"Return a copy with the first letter capitalized"
	| cap |
	self isEmpty ifTrue: [ ^self copy ].
	cap := self copy.
	cap at: 1 put: (cap at: 1) asUppercase.
	^ cap