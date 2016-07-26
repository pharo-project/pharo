drawing
show
	"Make sure this morph is on-stage."

	self visible
		ifTrue: [ ^ self ].
	self visible: true.
	self changed