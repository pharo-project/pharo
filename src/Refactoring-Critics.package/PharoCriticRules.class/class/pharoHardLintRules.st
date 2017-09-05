default rules
pharoHardLintRules
	"These rules are used by the CI monkey to check code before its integration into Pharo"
	^ { 
		RBUndeclaredReferenceRule.
		RBDefinesEqualNotHashRule.
		RBEquivalentSuperclassMethodsRule.
		RBJustSendsSuperRule.
		RBSubclassResponsibilityNotDefinedRule.
		"RBTempVarOverridesInstVarRule. gotta wait for a fix"
		RBVariableNotDefinedRule.
		RBEqualNotUsedRule.
		RBOnlyReadOrWrittenTemporaryRule.
		RBPrecedenceRule.
		OldSendsDeprecatedMethodToGlobalRule.
		RBSizeCheckRule.
		RBUnconditionalRecursionRule.
		RBUnoptimizedToDoRule.
		RBBetweenAndRule.
		RBCodeCruftLeftInMethodsRule.
		RBPharoBootstrapRule
	}