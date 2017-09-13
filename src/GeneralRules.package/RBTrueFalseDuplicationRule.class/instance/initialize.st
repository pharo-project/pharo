initialization
initialize
	super initialize.
	self
		"true/false last common"
		replace: '
		`.@PreStatements.
		`@boolExp 
			ifTrue: [ | `@trueTemps | `@.TrueStatements. `.LastStatement ] 
			ifFalse: [ | `@falseTemps | `@.FalseStatements. `.LastStatement ].
		`.@PostStatements.'
		with: '
		`.@PreStatements.
		`@boolExp 
			ifTrue: [ | `@trueTemps | `@.TrueStatements. ] 
			ifFalse: [ | `@falseTemps | `@.FalseStatements. ].
		`.LastStatement.
		`.@PostStatements.';
		
		"false/true last common"
		replace: '
		`.@PreStatements.
		`@boolExp 
			ifFalse: [ | `@falseTemps | `@.FalseStatements. `.LastStatement ] 
			ifTrue: [ | `@trueTemps | `@.TrueStatements. `.LastStatement ].
		`.@PostStatements.'
		with: '
		`.@PreStatements.
		`@boolExp 
			ifFalse: [ | `@falseTemps | `@.FalseStatements. ] 
			ifTrue: [ | `@trueTemps | `@.TrueStatements. ].
		`.LastStatement.
		`.@PostStatements.';
		
		"true/false first common"
		replace: '
		`.@PreStatements.
		`@boolExp 
			ifTrue: [ | `@trueTemps | `.FirstStatement. `@.TrueStatements. ] 
			ifFalse: [ | `@falseTemps | `.FirstStatement. `@.FalseStatements. ].
		`.@PostStatements.'
		with: '
		`.@PreStatements.
		`.FirstStatement.
		`@boolExp 
			ifTrue: [ | `@trueTemps | `@.TrueStatements. ] 
			ifFalse: [ | `@falseTemps | `@.FalseStatements. ].
		`.@PostStatements.';
		
		"false/true first common"
		replace: '
		`.@PreStatements.
		`@boolExp 
			ifFalse: [ | `@falseTemps | `.FirstStatement. `@.FalseStatements. ]
			ifTrue: [ | `@trueTemps | `.FirstStatement. `@.TrueStatements. ].
		`.@PostStatements.'
		with: '
		`.@PreStatements.
		`.FirstStatement.
		`@boolExp 
			ifFalse: [ | `@falseTemps | `@.FalseStatements. ]
			ifTrue: [ | `@trueTemps | `@.TrueStatements. ].
		`.@PostStatements.'