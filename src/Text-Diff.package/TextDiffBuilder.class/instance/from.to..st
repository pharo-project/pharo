initialize
from: xString to: yString

	xLines := (self split: xString asString) replace: [ :each | DiffElement string: each ].
	yLines := (self split: yString asString) replace: [ :each | DiffElement string: each ].
	self findMatches