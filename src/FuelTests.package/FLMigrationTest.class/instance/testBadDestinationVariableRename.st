tests-manual
testBadDestinationVariableRename
	
	| pointClass aPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	
	self materializer 
		migrateClassNamed: pointClassName 
		variables: {('x' -> 'posX')}.
		
	self serialize: aPoint.
	self should: [ self materialized ] raise: Error. 
	
	self flag: #todo."Assert an specific materialization error"