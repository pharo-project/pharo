private-packing
fillBuffer
	| packSize |
	packSize := self next.
	bufStream := (self next: packSize) readStream