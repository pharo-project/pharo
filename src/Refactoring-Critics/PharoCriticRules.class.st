"
I hold default rules used to check quality of code that is proposed for integration into Pharo.
"
Class {
	#name : #PharoCriticRules,
	#superclass : #Object,
	#category : #'Refactoring-Critics-DefaultRules'
}

{ #category : #'default rules' }
PharoCriticRules class >> pharoHardLintRules [
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
]

{ #category : #accessing }
PharoCriticRules class >> pharoIntegrationLintRule [
	"A set of rules used by the CI monkey to check code before its integration into Pharo"
	^ (RBCompositeLintRule rules: (self pharoHardLintRules collect: #new))
		name: 'Pharo integration rules';
		yourself
]

{ #category : #'default rules' }
PharoCriticRules class >> pharoSoftLintRules [	
	"skiped for now"
	^ {			
		"RBInstanceVariableCapitalizationRule.
		RBClassVariableCapitalizationRule.
		RBMissingSubclassResponsibilityRule.
		RBUnclassifiedMethodsRule.
		RBNoClassCommentRule. 
		RBOverridesSpecialMessageRule.
		RBRefersToClassRule.
		RBBadMessageRule.
		RBUsesTrueRule.
		RBSendsDifferentSuperRule.
		RBInconsistentMethodClassificationRule. 
		RBInstVarInSubclassesRule.
		RBSentNotImplementedRule.
		RBLongMethodsRule.
		RBCollectionProtocolRule.
		RBConsistencyCheckRule.
		RBContainsRule. RBDetectContainsRule.
		RBEmptyExceptionHandlerRule.
		RBEndTrueFalseRule.
		RBEqualsTrueRule.
		RBExtraBlockRule.
		RBFileBlocksRule.
		RBFloatEqualityComparisonRule.
		RBLiteralArrayCharactersRule.
		RBModifiesCollectionRule.
		RBReturnsIfTrueRule.
		RBSearchingLiteralRule.
		RBUnnecessaryAssignmentRule.
		RBNotEliminationRule.
		RBAtIfAbsentRule."
		"RBAllAnyNoneSatisfyRule."
		"RBClassNameInSelectorRule "
	}
]
