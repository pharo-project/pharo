tests - concatenation
testConcatenationWithEmpty
	| result |
	result:= self empty,self secondCollection .
	
	1 to: self secondCollection size do:
		[:i | 
		self assert: (self secondCollection at:i)= (result at:i).
		].
	"size : "
	self assert: result size = ( self secondCollection size).