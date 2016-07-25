accessing
method2
	| result tmpResult |
	
	result := self call: {1. 2. 3. 4}.
	tmpResult := result.
	result := FFICalloutObjectForTest basicNew.
	result instVarAt: 1 put: tmpResult.	
	^ result 
	