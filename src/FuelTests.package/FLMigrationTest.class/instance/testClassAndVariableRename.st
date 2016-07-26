tests-manual
testClassAndVariableRename
	
	| pointClass aPoint resultPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	aPoint instVarNamed: 'x' put: 7.	
	aPoint instVarNamed: 'y' put: 11.
	
	self serialize: aPoint.
	pointClass renameSilently: (pointClassName, 'Renamed') asSymbol.
	pointClass := self redefined: pointClass with: 'posY posX'.	
		
	self materializer 
		migrateClassNamed: pointClassName 
		toClass: pointClass
		variables: {'x' -> 'posX'. 'y' -> 'posY'}.
		
	resultPoint := self materialized.

	self assert: (resultPoint instVarNamed: 'posX') = 7.
	self assert: (resultPoint instVarNamed: 'posY') = 11.