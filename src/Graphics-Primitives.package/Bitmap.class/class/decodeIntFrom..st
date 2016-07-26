instance creation
decodeIntFrom: s 
	"Decode an integer in stream s as follows...
		0-223	0-223
		224-254	(0-30)*256 + next byte (0-7935)
		255		next 4 bytes	"
	| int |
	int := s next.
	int <= 223 ifTrue: [ ^ int ].
	int <= 254 ifTrue: [ ^ (int - 224) * 256 + s next ].
	int := s next.
	1 
		to: 3
		do: [ :j | int := (int bitShift: 8) + s next ].
	^ int