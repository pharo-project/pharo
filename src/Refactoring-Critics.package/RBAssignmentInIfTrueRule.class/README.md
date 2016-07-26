Moving assignments outside blocks leads to shorter and more efficient code.
For example:
test 
	ifTrue: [var := 1]
	ifFalse: [var:= 2]
is equivalent to:
var :=  test 
	ifTrue: [1]
	ifFalse: [2]