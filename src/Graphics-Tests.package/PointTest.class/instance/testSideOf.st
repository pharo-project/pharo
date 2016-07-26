tests - testing
testSideOf
	| p1 p2 p3 p4 p5 p6 o|
	
	p1:= 1@1.
	p2:= -1@(-1).
	p3:= -1@0.
	p4:= 0@1.
	p5:= 1@0.
	p6:=0@(-1).
	o := 0@0.
	
	self assert: (p1 sideOf:o) = #center.
	self assert: (p1 sideOf:p2) = #center.
	
	self assert: (p1 sideOf:p3) = #left.
	self assert:(p1 sideOf:p4) = #left.
	
	self assert: (p1 sideOf:p5) = #right.
	self assert:(p1 sideOf:p6) = #right.