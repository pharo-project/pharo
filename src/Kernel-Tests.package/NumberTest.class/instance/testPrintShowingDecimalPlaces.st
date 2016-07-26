tests
testPrintShowingDecimalPlaces
	self assert: (111.2 printShowingDecimalPlaces: 2) = '111.20'.
	self assert: (111.2 printShowingDecimalPlaces: 0) = '111'.
	self assert: (111 printShowingDecimalPlaces: 0) = '111'.
	self assert: (111111111111111 printShowingDecimalPlaces: 2) = '111111111111111.00'.
	self assert: (10 printShowingDecimalPlaces: 20) ='10.00000000000000000000'.
	self assert: (0.98 printShowingDecimalPlaces: 2) = '0.98'.
	self assert: (-0.98 printShowingDecimalPlaces: 2) = '-0.98'.
	self assert: (2.567 printShowingDecimalPlaces: 2) = '2.57'.
	self assert: (-2.567 printShowingDecimalPlaces: 2) = '-2.57'.
	self assert: (0.01 printShowingDecimalPlaces: 2) = '0.01'.
	self assert: (-0.001 printShowingDecimalPlaces: 2) = '0.00'.