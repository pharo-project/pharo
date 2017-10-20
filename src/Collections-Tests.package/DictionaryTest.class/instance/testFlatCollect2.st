tests - flatCollect
testFlatCollect2

	| res |
 	res := {#first -> #(-2 3). #second -> #(-4 5)} asDictionary flatCollect: [:e | e collect: #abs].
 	self assert: res asSet equals: #(2 3 4 5) asSet. 
	
 