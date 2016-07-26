tests
testNumbers
	| types tokens s |
	s := 'x 1'.
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1').
	
	s := 'x -1'.
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary #- number).
	self assert: tokens = #('x' '-' '1').

	s := 'x -1.1'.
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary #- number).
	self assert: tokens = #('x' '-' '1.1').

	s := 'x -1.1.'.
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary #- number statementSeparator).
	self assert: tokens = #('x' '-' '1.1' '.').
		
	s := 'x 1.true'.
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number statementSeparator #true).
	self assert: tokens = #('x' '1' '.' 'true').
		
	s := 'x 2r1'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2r1').

	s := 'x 2d1'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2d1').
	
	s := 'x 2e1'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2e1').
	
	s := 'x 2q1'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2q1').	
		
	s := 'x 16r-A'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r-A').

	s := 'x -16r-A'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary #- number).
	self assert: tokens = #('x' '-' '16r-A').
	
	s := 'x 16r'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r').
	
	s := 'x 16r-'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r-').

	s := 'x 16r-d'.	
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r-d').
			
	s := 'x 2r2'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number excessCode).
	self assert: tokens = #('x' '2r' '2').

	s := 'x 1.'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number statementSeparator).
	self assert: tokens = #('x' '1' '.').
	
	s := 'x 1yourself'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '1' 'yourself').
	
	s := 'x 1size'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '1' 'size').
	
	s := 'x 1.1size'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '1.1' 'size').	
	
	s := 'x 1s'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1s').
	
	s := 'x 1.1s'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.1s').	
	
	s := 'x 2r1e26'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2r1e26').
	
	s := 'x 16r1e-26'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number).
	self assert: tokens = #('x' '16r1e' '-' '26').
	
	s := 'x 16r1e'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1e').
	
	s := 'x 16r1e-1'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number).
	self assert: tokens = #('x' '16r1e' '-' '1').
		
	s := 'x 16r1e-'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary excessCode).
	self assert: tokens = #('x' '16r1e' '-' '').
	
	s := 'x 16r1.ABe20'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1.ABe20').
	
	s := 'x 16r1.ABe-20'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number).
	self assert: tokens = #('x' '16r1.ABe' '-' '20').

	s := 'x 1.0e14'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.0e14').
	
	s := 'x 1.0e-14'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.0e-14').
	
	s := 'x 1.0e14e'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '1.0e14' 'e').
""	
	s := 'x 2r1q26'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2r1q26').
	
	s := 'x 16r1q-26'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1q-26').
	
	s := 'x 16r1q'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '16r1' 'q').
	
	s := 'x 16r1q-1'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1q-1').
		
	s := 'x 16r1q-'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary binary excessCode).
	self assert: tokens = #('x' '16r1' 'q' '-' '').
	
	s := 'x 16r1.ABq20'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1.ABq20').
	
	s := 'x 16r1.ABq-20'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1.ABq-20').

	s := 'x 1.0q14'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.0q14').
	
	s := 'x 1.0q-14'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.0q-14').
	
	s := 'x 1.0q14q'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '1.0q14' 'q').
""
	s := 'x 2r1d26'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '2r1d26').
	
	s := 'x 16r1d-26'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number).
	self assert: tokens = #('x' '16r1d' '-' '26').
	
	s := 'x 16r1d'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1d').
	
	s := 'x 16r1d-1'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number).
	self assert: tokens = #('x' '16r1d' '-' '1').
		
	s := 'x 16r1d-'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary excessCode).
	self assert: tokens = #('x' '16r1d' '-' '').
	
	s := 'x 16r1.ABd20'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '16r1.ABd20').
	
	s := 'x 16r1.ABd-20'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number).
	self assert: tokens = #('x' '16r1.ABd' '-' '20').

	s := 'x 1.0d14'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.0d14').
	
	s := 'x 1.0d-14'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number).
	self assert: tokens = #('x' '1.0d-14').
	
	s := 'x 1.0d14d'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number unary).
	self assert: tokens = #('x' '1.0d14' 'd').
	
""

	s := 'x -1.1e-2'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary #- number ).
	self assert: tokens = #('x' '-' '1.1e-2'). "only first - is separate token !"
	
	s := 'x -16r-1.0e-2'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary #- number binary number).
	self assert: tokens = #('x' '-' '16r-1.0e' '-' '2'). "only first - is separate token !"
	
	s := 'x 1-1'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary number ).
	self assert: tokens = #('x' '1' '-' '1'). 

	s := 'x 1>-------1'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types equals: #(patternUnary number incompleteBinary number ).
	self assert: tokens equals: #('x' '1' '>-------' '1'). 

	s := 'x 1>-------"comment"1'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types equals: #(patternUnary number incompleteBinary comment number ).
	self assert: tokens equals: #('x' '1' '>-------' '"comment"' '1'). 	

	s := 'x 1-self'. 
	types := self tokenTypesIn: s.
	tokens := self tokensIn: s. 
	self assert: types = #(patternUnary number binary self ).
	self assert: tokens = #('x' '1' '-' 'self'). 