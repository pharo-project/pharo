tests - bitLogic
testTwoComplementRightShift
	"self run: #testTwoComplementRightShift"

	| large small |
	small := 2 << 16.
	large := 2 << 32.	
	self assert: ((small negated bitShift: -1) ~= ((small + 1) negated bitShift: -1)
		== ((large negated bitShift: -1) ~= ((large + 1) negated bitShift: -1))).
		
     self assert: ((small bitShift: -1) ~= (small + 1 bitShift: -1)
		== ((large bitShift: -1) ~= (large + 1 bitShift: -1))).