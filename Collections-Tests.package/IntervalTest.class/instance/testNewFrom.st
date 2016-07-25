tests
testNewFrom

	self assert: ( (Interval newFrom: (1 to: 1)) = (1 to: 1)).
	self assert: ( (Interval newFrom: #(1)) = (1 to: 1)).
	self assert: ( (Interval newFrom: #()) =  ( 1 to: 0)).