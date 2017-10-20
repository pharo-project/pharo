tests - geometry
testToIntersectsTo

	self assert: ( (1 @ 1) to: ( 1 @ 6) intersects: (1 @ 1) to: (2 @ 9) ).
	self assert: ( (1 @ 1) to: ( 1 @ 6) intersects: (1 @ 2) to: (1 @6) ).
	self assert: ( (1 @ 1) to: ( 2 @ 6) intersects: (8 @ 6) to: (1 @ 1) ).
	self assert: ( (5 @ 1) to: ( 1 @ 6) intersects: (1 @ 6) to: (9 @ 1) ).
	self deny:  ( (1 @ 1) to: ( 1 @ 6) intersects: (2 @ 1) to: (2 @ 6) ). 
	self deny:  ( (1 @ 1) to: ( 1 @ 6) intersects: (0 @7) to: (5 @ 7) ). 
	self assert: ( (1 @ 1) to: ( 1 @ 6) intersects: (0 @ 2) to: (2 @ 2) ). 
	
	self assert: ( (-1 @ -1) to: ( -1 @ -6) intersects: (-1 @ -1) to: (-2 @ -9) ).
	self assert: ( (-1 @ -1) to: ( -1 @ -6) intersects: (-1 @ -2) to: (-1 @ -6) ).
	self assert: ( (-1 @ -1) to: ( -2 @ -6) intersects: (-8 @ -6) to: (-1 @ -1) ).
	self assert: ( (-5 @ -1) to: ( -1 @ -6) intersects: (-1 @ -6) to: (-9 @ -1) ).
	self deny:  ( (-1 @ -1) to: (-1 @ -6) intersects: (-2 @ -1) to: (-2 @ -6) ). 
	self deny:  ( (-1 @ -1) to: ( -1 @ -6) intersects: (-0 @ -7) to: (-5 @ -7) ). 
	self assert: ( (-1 @ -1) to: ( -1 @ -6) intersects: (-0 @ -2) to: (-2 @ -2) ). 
	
	"point line( 1@1 to: 1@1 ) not allowed "
	self deny: ( (1 @ 1) to: ( 1 @ 1) intersects: (1@ 0) to: (1 @ 2) )."should be true if point line alowed" 
	self assert: ( (1 @ 1) to: ( 1 @ 1) intersects: (1 @ 1) to: (1 @ 9) ). 
