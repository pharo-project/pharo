attachments
totalLength
	"Answer the full length of my segments. Can take a long time if I'm curved."
	| length |
	length := 0.
	self lineSegmentsDo: [ :a :b | length := length + (a distanceTo: b) ].
	^ length.