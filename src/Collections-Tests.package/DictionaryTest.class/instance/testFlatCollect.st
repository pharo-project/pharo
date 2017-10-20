tests - flatCollect
testFlatCollect

	| res |
 	res := {#first -> -1. #second -> 5 . #three -> -33} asDictionary flatCollect: [:e | { e abs } ].
 	self assert: res asSet equals: #(1 5 33) asSet. 
	
 