accessing
step
	borderDashSpec ifNil: [^super step].
	borderDashSpec size < 5 ifTrue: [^super step].

	"Only for dashed lines with creep"
	borderDashSpec at: 4 put: (borderDashSpec fourth) + borderDashSpec fifth.
	self changed.
	^ super step