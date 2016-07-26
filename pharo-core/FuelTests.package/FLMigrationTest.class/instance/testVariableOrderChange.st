tests-automatic
testVariableOrderChange
	"Tests that serializer tolarates when the order in the instance variables changed between serialization and materialization"
	
	| pairClass aPair resultPair |	
	pairClass := self newClassWithInstanceVariableNames: 'left right'.
	aPair := pairClass new.
	aPair instVarAt: 1 put: $A.	
	aPair instVarAt: 2 put: $B.	
	
	self serialize: aPair.
	self redefined: pairClass with: 'right left'.
	resultPair := self materialized.

	self assert: $B = (resultPair instVarAt: 1).
	self assert: $A = (resultPair instVarAt: 2).	