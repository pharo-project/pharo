tests - testing
testBasicFourDirections
	"fourDirections returns the four rotation of the receiver in counter clockwise order with the receiver appearing last."
	
	| samples results rejects |
	self assert:  (0@0) fourDirections = {(0@0). (0@0). (0@0). (0@0)}.
	self assert: (0@0) eightNeighbors =  {(1@0). (1@1). (0@1). (-1@1). (-1@0). (-1@ -1). (0@ -1). (1@ -1)}.

	samples :=  {(1@0). (1@1). (0@1). (-1@1). (-1@0). (-1@ -1). (0@ -1). (1@ -1)}.
	results :=  { {0@ -1 . -1@0 . 0@1 . 1@0} 
 				. {1@ -1 . -1@ -1 . -1@1 . 1@1}  
 				. {1@0 . 0@ -1 . -1@0 . 0@1}  
 				. {1@1 . 1@ -1 . -1@ -1 . -1@1}  
 				. {0@1 . 1@0 . 0@ -1 . -1@0}  
 				. {-1@1 . 1@1 . 1@ -1 . -1@ -1}  
 				. {-1@0 . 0@1 . 1@0 . 0@ -1}  
 				. {-1@ -1 . -1@1 . 1@1 . 1@ -1} } .

	rejects := (1 to: samples size ) reject: [ :each |
											 (samples at: each) fourDirections = (results at: each) ].
	self assert: ( rejects isEmpty).
