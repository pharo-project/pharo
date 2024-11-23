"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfUI

Load type: 
- atomic

Installs:
- SUnit UI
- Nautilus
- NECompletion
- ProcessBrowsers
- profillers
- different Metacello repositories
- Monticello GUI
- ProfStef
- Athens
..and some other packages
"
Class {
	#name : 'BaselineOfBasicTools',
	#superclass : 'BaselineOf',
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfBasicTools',
	#package : 'BaselineOfBasicTools'
}

{ #category : 'baselines' }
BaselineOfBasicTools >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.

		"Load first SUnit core"
		spec baseline: 'SUnitCore' with: [
			spec
				loads: 'Core';
				className: 'BaselineOfSUnit';
				repository: repository ].
		spec
			baseline: 'UI' with: [ spec repository: repository ];
			baseline: 'Reflectivity' with: [ spec repository: repository ];
			baseline: 'DebugPoints' with: [ spec repository: repository ];
			baseline: 'Athens' with: [
				spec
					loads: 'Cairo-core';
					repository: repository ];
			baseline: 'SUnit' with: [
			spec
				loads: #( 'UI' 'JenkinsSupport' );
				repository: repository ].
		spec package: 'Tool-ExternalBrowser'.
		spec baseline: 'EnlumineurFormatter' with: [
			spec
				loads: 'Core';
				repository: repository ].

		spec package: 'Tool-ProcessBrowser'.
		spec package: 'Tool-Profilers'.
		spec package: 'NECompletion'.
		spec package: 'NECompletion-Morphic'.
		spec package: 'NECompletion-Preferences'.
		spec package: 'Metacello-FileTree'.
		spec package: 'Metacello-Cypress'.
		spec package: 'CodeExport'.
		spec package: 'MonticelloGUI' with: [ spec requires: #( 'Tool-Diff' ) ].
		spec package: 'System-Sources-Tests'. "<= Not sure this one should be here but it is where the classes were loaded before been extracted from Tests package."

		spec package: 'Kernel-CodeModel-Tests'.
		spec package: 'Monticello-Tests'.
		spec package: 'MonticelloGUI-Tests'.
		spec package: 'Network-Mail'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'ProfStef-Core'.
		spec package: 'StartupPreferences'.

		spec baseline: 'Refactoring' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].

		spec package: 'Monticello-BackwardCompatibility'.
		spec package: 'MonticelloFileServices'.

		self specRefactoring: spec.

		spec package: 'Tools-CodeNavigation'.
		spec package: 'Tool-Diff'.
		spec package: 'Tool-ImageCleaner'.
		spec package: 'Tools' ]
]

{ #category : 'actions' }
BaselineOfBasicTools >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	"This should be removed in Pharo 14."
	ASTNode deprecatedAliases: { #RBNode }.
	ASTProgramNode deprecatedAliases: { #RBProgramNode }.
	ASTValueNode deprecatedAliases: { #RBValueNode }.
	ASTLiteralNode deprecatedAliases: { #RBLiteralNode }.
	ASTLiteralArrayNode deprecatedAliases: { #RBLiteralArrayNode }.
	ASTLiteralValueNode deprecatedAliases: { #RBLiteralValueNode }.
	ASTArrayNode deprecatedAliases: { #RBArrayNode }.
	ASTAssignmentNode deprecatedAliases: { #RBAssignmentNode }.
	ASTCascadeNode deprecatedAliases: { #RBCascadeNode }.
	ASTReturnNode deprecatedAliases: { #RBReturnNode }.
	ASTVariableNode deprecatedAliases: { #RBVariableNode }.
	ASTCommentNode deprecatedAliases: { #RBComment }.
	ASTBlockNode deprecatedAliases: { #RBBlockNode }.
	ASTMessageNode deprecatedAliases: { #RBMessageNode }.
	ASTSelectorNode deprecatedAliases: { #RBSelectorNode }.
	ASTPragmaNode deprecatedAliases: { #RBPragmaNode }.
	ASTSequenceNode deprecatedAliases: { #RBSequenceNode }.
	ASTAnnotationMarkNode deprecatedAliases: { #RBAnnotationMarkNode }.
	ASTMethodNode deprecatedAliases: { #RBMethodNode }.
	ASTDoItMethodNode deprecatedAliases: { #RBDoItMethodNode }.
	ASTEnglobingErrorNode deprecatedAliases: { #RBEnglobingErrorNode }.
	ASTArrayErrorNode deprecatedAliases: { #RBArrayErrorNode }.
	ASTUnfinishedStatementErrorNode deprecatedAliases: { #RBUnfinishedStatementErrorNode }.
	ASTLiteralByteArrayErrorNode deprecatedAliases: { #RBLiteralByteArrayErrorNode }.
	ASTPragmaErrorNode deprecatedAliases: { #RBPragmaErrorNode }.
	ASTLiteralArrayErrorNode deprecatedAliases: { #RBLiteralArrayErrorNode }.
	ASTParenthesesErrorNode deprecatedAliases: { #RBParenthesesErrorNode }.
	ASTAssignmentErrorNode deprecatedAliases: { #RBAssignmentErrorNode }.
	ASTBlockErrorNode deprecatedAliases: { #RBBlockErrorNode }.
	ASTTemporariesErrorNode deprecatedAliases: { #RBTemporariesErrorNode }.
	ASTInvalidCascadeErrorNode deprecatedAliases: { #RBInvalidCascadeErrorNode }.
	ASTParseErrorNode deprecatedAliases: { #RBParseErrorNode }.
	
	ASTAbstractFormatter deprecatedAliases: { #RBAbstractFormatter }.
	ASTSimpleFormatter deprecatedAliases: { #RBSimpleFormatter }.
	
	ASTParseTreeSearcher deprecatedAliases: { #RBParseTreeSearcher }.
	ASTParseTreeRule deprecatedAliases: { #RBParseTreeRule }.
	ASTReplaceRule deprecatedAliases: { #RBReplaceRule }.
	ASTSearchRule deprecatedAliases: { #RBSearchRule }.
	ASTBlockReplaceRule deprecatedAliases: { #RBBlockReplaceRule }.
	ASTParseTreeRewriter deprecatedAliases: { #RBParseTreeRewriter }.
	ASTStringReplacement deprecatedAliases: { #RBStringReplacement }.
	ASTStringReplaceRule deprecatedAliases: { #RBStringReplaceRule }.
	
	ASTReadBeforeWrittenTester deprecatedAliases: { #RBReadBeforeWrittenTester }.
	ASTProgramNodeVisitor deprecatedAliases: { #RBProgramNodeVisitor }.
	ASTReturnNodeAdderVisitor deprecatedAliases: { #RBReturnNodeAdderVisitor }.
	ASTParseErrorNodeVisitor deprecatedAliases: { #RBParseErrorNodeVisitor }.
	ASTCommentNodeVisitor deprecatedAliases: { #RBCommentNodeVisitor }.
	ASTGenericNodeVisitor deprecatedAliases: { #RBGenericNodeVisitor }.
	ASTDumpVisitor deprecatedAliases: { #RBDumpVisitor }.
	ASTAbstractBlockVisitor deprecatedAliases: { #RBAbstractBlockVisitor }.
	
	ASTErrorNotice deprecatedAliases: { #RBErrorNotice }.
	ASTWarningNotice deprecatedAliases: { #RBWarningNotice }.
	ASTSyntaxErrorNotice deprecatedAliases: { #RBSyntaxErrorNotice }.
	ASTNotice deprecatedAliases: { #RBNotice }.
	
	ASTCommentToken deprecatedAliases: { #RBCommentToken}.
	ASTLiteralToken deprecatedAliases: { #RBLiteralToken}.
	ASTSpecialCharacterToken deprecatedAliases: { #RBSpecialCharacterToken}.
	ASTIdentifierToken deprecatedAliases: { #RBIdentifierToken}.
	ASTNumberLiteralToken deprecatedAliases: { #RBNumberLiteralToken}.
	ASTToken deprecatedAliases: { #RBToken}.
	ASTBinarySelectorToken deprecatedAliases: { #RBBinarySelectorToken}.
	ASTKeywordToken deprecatedAliases: { #RBKeywordToken}.
	ASTAssignmentToken deprecatedAliases: { #RBAssignmentToken}.
	ASTErrorToken deprecatedAliases: { #RBErrorToken}.
	ASTValueToken deprecatedAliases: { #RBValueToken}.
	ASTEOFToken deprecatedAliases: { #RBEOFToken}.
	ASTLiteralArrayToken deprecatedAliases: { #RBLiteralArrayToken}.
	ASTPatternBlockToken deprecatedAliases: { #RBPatternBlockToken}.

	ASTPatternMessageNode deprecatedAliases: { #RBPatternMessageNode }.
	ASTPatternBlockNode deprecatedAliases: { #RBPatternBlockNode }.
	ASTPatternWrapperBlockNode deprecatedAliases: { #RBPatternWrapperBlockNode }.
	ASTPatternMethodNode deprecatedAliases: { #RBPatternMethodNode }.
	ASTPatternVariableNode deprecatedAliases: { #RBPatternVariableNode }.
	ASTPatternPragmaNode deprecatedAliases: { #RBPatternPragmaNode }.
	
	CompletionSorter register.
	RubSmalltalkEditor completionEngineClass: CompletionEngine.

	Initialized := true.
]

{ #category : 'baseline' }
BaselineOfBasicTools >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
]

{ #category : 'baseline' }
BaselineOfBasicTools >> specRefactoring: spec [
	
	"Package used to rename #defaultSpec methods to #defaultLayout.
	Should be removed once users of #defaultSpec are migrated."
	
	spec baseline: 'SpecRefactoring' with: [
		spec 
			repository: (self class environment at: #BaselineOfPharo) specRepository;
			className: 'BaselineOfSpec2';
			loads: #('Spec2-Transformations') ]
]
