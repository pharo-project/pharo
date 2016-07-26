tests
testPointPointConstructorIsSymmetrical
	| pt1 pt2 rect1 rect2 |

	pt1:= 0@0.
	pt2 := 10@10.
    
	rect1 := Rectangle point: pt1 point: pt2.
	rect2 := Rectangle point: pt2 point: pt1.
	
	self assert: rect1 equals: rect2