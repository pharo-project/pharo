truncation and round off
truncated
	"Answer a Rectangle whose origin and corner have any fractional parts removed. Answer the receiver if its coordinates are already integral."

	(origin x isInteger and:
	[origin y isInteger and:
	[corner x isInteger and:
	[corner y isInteger]]])
		ifTrue: [^ self].

	^ Rectangle origin: origin truncated corner: corner truncated
