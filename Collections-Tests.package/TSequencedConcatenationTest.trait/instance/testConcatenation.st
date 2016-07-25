tests - concatenation
testConcatenation
	| result index |
	result:= self firstCollection,self secondCollection .
	"first part : "
	index := 1.
	self firstCollection do: 
		[:each | 
		self assert: (self firstCollection at: index)=each.
		index := index+1.].
	"second part : "
	1 to: self secondCollection size do:
		[:i | 
		self assert: (self secondCollection at:i)= (result at:index).
		index:=index+1].
	"size : "
	self assert: result size = (self firstCollection size + self secondCollection size).