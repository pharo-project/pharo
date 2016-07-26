tests-manual
testClassRename
	
	| pointClass aPoint resultPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	aPoint instVarNamed: 'x' put: 7.	
	aPoint instVarNamed: 'y' put: 11.
	
	self serialize: aPoint.
	pointClass renameSilently: (pointClassName, 'Renamed') asSymbol.

	self materializer 
		migrateClassNamed: pointClassName 
		toClass: pointClass.
		
	resultPoint := self materialized.

	self assert: (resultPoint instVarNamed: 'x') = 7.
	self assert: (resultPoint instVarNamed: 'y') = 11.