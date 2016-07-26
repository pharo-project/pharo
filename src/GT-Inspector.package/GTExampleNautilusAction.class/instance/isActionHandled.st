order
isActionHandled
	^ method pragmas anySatisfy: [ :each | each isGTExamplePragma ]