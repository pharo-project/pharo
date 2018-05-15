"
SUnit tests for Shouts ST80 parser
"
Class {
	#name : #SHParserST80Test,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #utilities }
SHParserST80Test >> >-------> anObject [

"Dummy binary selector method"
]

{ #category : #manual }
SHParserST80Test >> d [
	"ensure that d is defined so that we don't get an #undefinedUnary token type "
]

{ #category : #manual }
SHParserST80Test >> e [
	"ensure that e is defined so that we don't get an #undefinedUnary token type "
]

{ #category : #manual }
SHParserST80Test >> q [
	"ensure that q is defined so that we don't get an #undefinedUnary token type "
]

{ #category : #manual }
SHParserST80Test >> r [
	"ensure that r is defined so that we don't get an #undefinedUnary token type "
]

{ #category : #manual }
SHParserST80Test >> s [
	"ensure that s is defined so that we don't get an #undefinedUnary token type "
]

{ #category : #'tests - smoke' }
SHParserST80Test >> testBooleanHierarchy [
	self verifyHierarchy: Boolean
]

{ #category : #'tests - smoke' }
SHParserST80Test >> testCollectionHierarchy [
	self skip: 'Too slow for too little value added'.
	self verifyHierarchy: Collection
]

{ #category : #'tests - smoke' }
SHParserST80Test >> testNumberHierarchy [
	self verifyHierarchy: Number
]

{ #category : #tests }
SHParserST80Test >> testNumbers [
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
]

{ #category : #'tests - smoke' }
SHParserST80Test >> testObjectClass [
	self verifyClass: Object
]

{ #category : #utilities }
SHParserST80Test >> tokenTypesIn: aString [
	| parser ranges |
	parser := SHParserST80 new.
	ranges := parser rangesIn: aString classOrMetaClass: Object workspace: nil environment: nil.
	^ranges asArray collect: [:each | each type]
]

{ #category : #utilities }
SHParserST80Test >> tokensIn: aString [
	| parser ranges |
	parser := SHParserST80 new.
	ranges := parser rangesIn: aString classOrMetaClass: Object workspace: nil environment: nil.
	^ranges asArray collect: [:each | aString copyFrom: each start to: each end]
]

{ #category : #utilities }
SHParserST80Test >> verifyClass: aBehavior [
	aBehavior selectors do: [ :selector | self verifyClass: aBehavior selector: selector ]
	
]

{ #category : #utilities }
SHParserST80Test >> verifyClass: aBehavior selector: aSymbol [
	| ranges position types errors |
	ranges := SHParserST80 new 
		rangesIn: (aBehavior sourceCodeAt: aSymbol) 
		classOrMetaClass: aBehavior 
		workspace: nil environment: nil.
	types := #(#default #invalid #excessCode #comment #unfinishedComment #'$' #character #integer #number #- #symbol #stringSymbol #literalArray #string #unfinishedString #assignment #ansiAssignment #literal #keyword #binary #unary #incompleteKeyword #incompleteBinary #incompleteUnary #undefinedKeyword #undefinedBinary #undefinedUnary #patternKeyword #patternBinary #patternUnary #self #super #true #false #nil #thisContext #return #patternArg #methodArg #blockPatternArg #blockArg #argument #blockArgColon #leftParenthesis #rightParenthesis #leftParenthesis1 #rightParenthesis1 #leftParenthesis2 #rightParenthesis2 #leftParenthesis3 #rightParenthesis3 #leftParenthesis4 #rightParenthesis4 #leftParenthesis5 #rightParenthesis5 #leftParenthesis6 #rightParenthesis6 #leftParenthesis7 #rightParenthesis7 #blockStart #blockEnd #blockStart1 #blockEnd1 #blockStart2 #blockEnd2 #blockStart3 #blockEnd3 #blockStart4 #blockEnd4 #blockStart5 #blockEnd5 #blockStart6 #blockEnd6 #blockStart7 #blockEnd7 #arrayStart #arrayEnd #arrayStart1 #arrayEnd1 #byteArrayStart #byteArrayEnd #byteArrayStart1 #byteArrayEnd1 #leftBrace #rightBrace #cascadeSeparator #statementSeparator #externalCallType #externalCallTypePointerIndicator #primitiveOrExternalCallStart #primitiveOrExternalCallEnd #methodTempBar #blockTempBar #blockArgsBar #primitive #externalFunctionCallingConvention #module #blockTempVar #blockPatternTempVar #instVar #workspaceVar #undefinedIdentifier #incompleteIdentifier #tempVar #patternTempVar #poolConstant #classVar #globalVar #pragmaKeyword #pragmaUnary #pragmaBinary) asIdentitySet.
	errors := #(#excessCode #unfinishedComment #unfinishedString) asIdentitySet.
	position := 0.
	ranges do: [ :each |
		self 
			assert: 1 <= each length 
			description: 'empty range'.
		self 
			assert: position <= each start
			description: 'overlapping range'.
		self 
			assert: each start <= each end 
			description: 'empty range'.
		self 
			assert: (types includes: each type)
			description: 'unknown type ' , each type.
		self
			deny: (errors includes: each type)
			description: 'error type ' , each type.
		position := each end ]
]

{ #category : #utilities }
SHParserST80Test >> verifyHierarchy: aBehavior [
	aBehavior withAllSubclassesDo: [ :class | self verifyClass: class; verifyClass: class class ]
]
