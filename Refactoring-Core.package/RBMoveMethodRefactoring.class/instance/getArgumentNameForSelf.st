transforming
getArgumentNameForSelf
	self needsToReplaceSelfReferences ifFalse: [^self].
	
	[selfVariableName := self requestSelfArgumentName.
	(self checkInstanceVariableName: selfVariableName in: class) 
		ifTrue: 
			[self verifyTemporaryVariableDoesNotOverride 
				ifFalse: 
					[self 
						refactoringWarning: 'The variable is already defined in one of the classes you''re moving the method to.<n>Try another?' 
								expandMacros.
					selfVariableName := nil]]
		ifFalse: 
			[self 
				refactoringWarning: 'The variable name is not a valid Smalltalk temporary variable name<n>Try again?' 
						expandMacros.
			selfVariableName := nil].
	selfVariableName isNil] 
			whileTrue: []