tests
testScaleBy
	| rect |
	rect:= (2@2 corner: 6@4). 
	
	self assert: (rect scaleBy: 2) left = 4.
	self assert: (rect scaleBy: 2) right = 12.
	self assert: (rect scaleBy: 2) top = 4.
	self assert: (rect scaleBy: 2) bottom = 8.
	
	self assert: (rect scaleBy: (1/2)) left = 1.
	self assert: (rect scaleBy: (1/2)) right = 3.
	self assert: (rect scaleBy: (1/2)) top = 1.
	self assert: (rect scaleBy: (1/2)) bottom = 2.
	
	self assert: (rect scaleBy: 0) left = 0.
	self assert: (rect scaleBy: 0) right = 0.
	self assert: (rect scaleBy: 0) top = 0.
	self assert: (rect scaleBy: 0) bottom = 0.