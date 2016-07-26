read, write, position
truncate: pos
	"Truncate to this position"

	self position: pos.
	^self primTruncate: fileID to: pos