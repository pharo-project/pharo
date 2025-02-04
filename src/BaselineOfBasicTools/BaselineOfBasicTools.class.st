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
		spec package: 'MonticelloMocks'.
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
	 OCNode deprecatedAliases: { #RBNode. #ASTNode }.
	 OCProgramNode deprecatedAliases: { #RBProgramNode. #ASTProgramNode }.
	 OCValueNode deprecatedAliases: { #RBValueNode. #ASTValueNode }.
	 OCLiteralNode deprecatedAliases: { #RBLiteralNode. #ASTLiteralNode }.
	 OCLiteralArrayNode deprecatedAliases:
		 { #RBLiteralArrayNode. #ASTLiteralArrayNode }.
	 OCLiteralValueNode deprecatedAliases:
		 { #RBLiteralValueNode. #ASTLiteralValueNode }.
	 OCArrayNode deprecatedAliases: { #RBArrayNode. #ASTArrayNode }.
	 OCAssignmentNode deprecatedAliases: {
			 #RBAssignmentNode.
			 #ASTAssignmentNode }.
	 OCCascadeNode deprecatedAliases: { #RBCascadeNode. #ASTCascadeNode }.
	 OCReturnNode deprecatedAliases: { #RBReturnNode. #ASTReturnNode }.
	 OCVariableNode deprecatedAliases:
		 { #RBVariableNode. #ASTVariableNode }.
	 OCCommentNode deprecatedAliases: { #RBComment. #ASTComment }.
	 OCBlockNode deprecatedAliases: { #RBBlockNode. #ASTBlockNode }.
	 OCMessageNode deprecatedAliases: { #RBMessageNode. #ASTMessageNode }.
	 OCSelectorNode deprecatedAliases:
		 { #RBSelectorNode. #ASTSelectorNode }.
	 OCPragmaNode deprecatedAliases: { #RBPragmaNode. #ASTPragmaNode }.
	 OCSequenceNode deprecatedAliases:
		 { #RBSequenceNode. #ASTSequenceNode }.
	 OCAnnotationMarkNode deprecatedAliases:
		 { #RBAnnotationMarkNode. #ASTAnnotationMarkNode }.
	 OCMethodNode deprecatedAliases: { #RBMethodNode. #ASTMethodNode }.
	 OCDoItMethodNode deprecatedAliases:
		 { #RBDoItMethodNode. #ASTDoItMethodNode }.
	 OCEnglobingErrorNode deprecatedAliases:
		 { #RBEnglobingErrorNode. #ASTEnglobingErrorNode }.
	 OCArrayErrorNode deprecatedAliases:
		 { #RBArrayErrorNode. #ASTArrayErrorNode }.
	 OCUnfinishedStatementErrorNode deprecatedAliases:
		 { #RBUnfinishedStatementErrorNode. #ASTUnfinishedStatementErrorNode }.
	 OCLiteralByteArrayErrorNode deprecatedAliases:
		 { #RBLiteralByteArrayErrorNode. #ASTLiteralByteArrayErrorNode }.
	 OCPragmaErrorNode deprecatedAliases:
		 { #RBPragmaErrorNode. #ASTPragmaErrorNode }.
	 OCLiteralArrayErrorNode deprecatedAliases:
		 { #RBLiteralArrayErrorNode. #ASTLiteralArrayErrorNode }.
	 OCParenthesesErrorNode deprecatedAliases:
		 { #RBParenthesesErrorNode. #ASTParenthesesErrorNode }.
	 OCAssignmentErrorNode deprecatedAliases:
		 { #RBAssignmentErrorNode. #ASTAssignmentErrorNode }.
	 OCBlockErrorNode deprecatedAliases:
		 { #RBBlockErrorNode. #ASTBlockErrorNode }.
	 OCTemporariesErrorNode deprecatedAliases:
		 { #RBTemporariesErrorNode. #ASTTemporariesErrorNode }.
	 OCInvalidCascadeErrorNode deprecatedAliases:
		 { #RBInvalidCascadeErrorNode. #ASTInvalidCascadeErrorNode }.
	 OCParseErrorNode deprecatedAliases:
		 { #RBParseErrorNode. #ASTParseErrorNode }.

	 OCAbstractFormatter deprecatedAliases:
		 { #RBAbstractFormatter. #ASTAbstractFormatter }.
	 OCSimpleFormatter deprecatedAliases:
		 { #RBSimpleFormatter. #ASTSimpleFormatter }.

	 OCParseTreeSearcher deprecatedAliases:
		 { #RBParseTreeSearcher. #ASTParseTreeSearcher }.
	 OCParseTreeRule deprecatedAliases:
		 { #RBParseTreeRule. #ASTParseTreeRule }.
	 OCReplaceRule deprecatedAliases: { #RBReplaceRule. #ASTReplaceRule }.
	 OCSearchRule deprecatedAliases: { #RBSearchRule. #ASTSearchRule }.
	 OCBlockReplaceRule deprecatedAliases:
		 { #RBBlockReplaceRule. #ASTBlockReplaceRule }.
	 OCParseTreeRewriter deprecatedAliases:
		 { #RBParseTreeRewriter. #ASTParseTreeRewriter }.
	 OCStringReplacement deprecatedAliases:
		 { #RBStringReplacement. #ASTStringReplacement }.
	 OCStringReplaceRule deprecatedAliases:
		 { #RBStringReplaceRule. #ASTStringReplaceRule }.

	 OCReadBeforeWrittenTester deprecatedAliases:
		 { #RBReadBeforeWrittenTester. #ASTReadBeforeWrittenTester }.
	 OCProgramNodeVisitor deprecatedAliases:
		 { #RBProgramNodeVisitor. #ASTProgramNodeVisitor }.
	 OCReturnNodeAdderVisitor deprecatedAliases:
		 { #RBReturnNodeAdderVisitor. #ASTReturnNodeAdderVisitor }.
	 OCParseErrorNodeVisitor deprecatedAliases:
		 { #RBParseErrorNodeVisitor. #ASTParseErrorNodeVisitor }.
	 OCCommentNodeVisitor deprecatedAliases:
		 { #RBCommentNodeVisitor. #ASTCommentNodeVisitor }.
	 OCGenericNodeVisitor deprecatedAliases:
		 { #RBGenericNodeVisitor. #ASTGenericNodeVisitor }.
	 OCDumpVisitor deprecatedAliases: { #RBDumpVisitor. #ASTDumpVisitor }.
	 OCAbstractBlockVisitor deprecatedAliases:
		 { #RBAbstractBlockVisitor. #ASTAbstractBlockVisitor }.

	 OCErrorNotice deprecatedAliases: { #RBErrorNotice. #ASTErrorNotice }.
	 OCWarningNotice deprecatedAliases:
		 { #RBWarningNotice. #ASTWarningNotice }.
	 OCSyntaxErrorNotice deprecatedAliases:
		 { #RBSyntaxErrorNotice. #ASTSyntaxErrorNotice }.
	 OCNotice deprecatedAliases: { #RBNotice. #ASTNotice }.

	 OCCommentToken deprecatedAliases:
		 { #RBCommentToken. #ASTCommentToken }.
	 OCLiteralToken deprecatedAliases:
		 { #RBLiteralToken. #ASTLiteralToken }.
	 OCSpecialCharacterToken deprecatedAliases:
		 { #RBSpecialCharacterToken. #ASTSpecialCharacterToken }.
	 OCIdentifierToken deprecatedAliases:
		 { #RBIdentifierToken. #ASTIdentifierToken }.
	 OCNumberLiteralToken deprecatedAliases:
		 { #RBNumberLiteralToken. #ASTNumberLiteralToken }.
	 OCToken deprecatedAliases: { #RBToken. #ASTToken }.
	 OCBinarySelectorToken deprecatedAliases:
		 { #RBBinarySelectorToken. #ASTBinarySelectorToken }.
	 OCKeywordToken deprecatedAliases:
		 { #RBKeywordToken. #ASTKeywordToken }.
	 OCAssignmentToken deprecatedAliases:
		 { #RBAssignmentToken. #ASTAssignmentToken }.
	 OCErrorToken deprecatedAliases: { #RBErrorToken. #ASTErrorToken }.
	 OCValueToken deprecatedAliases: { #RBValueToken. #ASTValueToken }.
	 OCEOFToken deprecatedAliases: { #RBEOFToken. #ASTEOFToken }.
	 OCLiteralArrayToken deprecatedAliases:
		 { #RBLiteralArrayToken. #ASTLiteralArrayToken }.
	 OCPatternBlockToken deprecatedAliases:
		 { #RBPatternBlockToken. #ASTPatternBlockToken }.

	 OCPatternMessageNode deprecatedAliases:
		 { #RBPatternMessageNode. #ASTPatternMessageNode }.
	 OCPatternBlockNode deprecatedAliases:
		 { #RBPatternBlockNode. #ASTPatternBlockNode }.
	 OCPatternWrapperBlockNode deprecatedAliases:
		 { #RBPatternWrapperBlockNode. #ASTPatternWrapperBlockNode }.
	 OCPatternMethodNode deprecatedAliases:
		 { #RBPatternMethodNode. #ASTPatternMethodNode }.
	 OCPatternVariableNode deprecatedAliases:
		 { #RBPatternVariableNode. #ASTPatternVariableNode }.
	 OCPatternPragmaNode deprecatedAliases:
		 { #RBPatternPragmaNode. #ASTPatternPragmaNode }.

	OCDoItVariable deprecatedAliases: { #DoItVariable }.

	 CompletionSorter register.
	 RubSmalltalkEditor completionEngineClass: CompletionEngine.

	 Initialized := true
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
