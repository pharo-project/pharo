tests - point functions
testInsideTriangleWithWith
	self assert:   ( (2 @ 3) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self deny:    ( (3 @ 1) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self deny:    ( (3 @ 15) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self deny:    ( (0 @ 6) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self assert:   ( (-2 @ -3) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
	self deny:    ( (-3 @ -1) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
	self deny:    ( (-3 @ -15) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
	self deny:    ( (0 @ -6) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).