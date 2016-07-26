tests-automatic
testVariableInsertion
	"Tests that serializer tolarates when there is a new instance variable on materialization"
	
	| pairClass aPair resultPair |	
	pairClass := self newClassWithInstanceVariableNames: 'left right'.
	aPair := pairClass new.
	aPair instVarAt: 1 put: $A.	
	aPair instVarAt: 2 put: $B.
	
	self serialize: aPair.
	self redefined: pairClass with: 'left middle right'.
	resultPair := self materialized.

	self assert: $A = (resultPair instVarAt: 1).
	self assert: nil = (resultPair instVarAt: 2).
	self assert: $B = (resultPair instVarAt: 3).