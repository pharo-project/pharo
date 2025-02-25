"
Manifest for AST-Core package
"
Class {
	#name : 'ManifestASTCore',
	#superclass : 'PackageManifest',
	#category : 'AST-Core-Manifest',
	#package : 'AST-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestASTCore class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #( #'OpalCompiler-Core')
]

{ #category : 'meta data' }
ManifestASTCore class >> rejectRules [
^ #('LawOfDemeterRule')
]

{ #category : 'meta data' }
ManifestASTCore class >> ruleEquivalentSuperclassMethodsRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCProgramNode #isBlock #false)) #'2013-03-29T18:06:26.929+01:00') )
]

{ #category : 'meta data' }
ManifestASTCore class >> ruleLongMethodsRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#OCParser #parseBlockArgsInto: #false)) #'2013-03-29T18:06:27.028000001+01:00') #(#(#RGMethodDefinition #(#OCParser #patchLiteralArrayToken #false)) #'2013-03-29T18:06:27.030000002+01:00') #(#(#RGMethodDefinition #(#RBConfigurableFormatter #needsParenthesisFor: #false)) #'2013-03-29T18:06:27.030000009+01:00') #(#(#RGMethodDefinition #(#OCParser #parsePrimitiveObject #false)) #'2013-03-29T18:06:27.028000002+01:00') #(#(#RGMethodDefinition #(#RBFormatter #needsParenthesisFor: #false)) #'2013-03-29T18:06:27.028+01:00') #(#(#RGMethodDefinition #(#RBFormatter #formatMessage:cascade: #false)) #'2013-03-29T18:06:27.03+01:00') #(#(#RGMethodDefinition #(#'RBConfigurableFormatter class' #initialize #true)) #'2013-03-29T18:06:27.030000001+01:00') #(#(#RGMethodDefinition #(#OCParseTreeRewriter #visitCascadeNode: #false)) #'2013-03-29T18:06:27.030000004+01:00') #(#(#RGMethodDefinition #(#'RBConfigurableFormatter class' #settingsOn: #true)) #'2013-03-29T18:06:27.030000006+01:00') #(#(#RGMethodDefinition #(#OCProgramNode #matchList:index:against:index:inContext: #false)) #'2013-03-29T18:06:27.030000008+01:00') #(#(#RGMethodDefinition #(#OCMessageNode #replaceSourceWithMessageNode: #false)) #'2013-03-29T18:06:27.030000003+01:00') #(#(#RGMethodDefinition #(#OCParser #parseCascadeMessage #false)) #'2013-03-29T18:06:27.030000007+01:00') )
]

{ #category : 'code-critics' }
ManifestASTCore class >> ruleReCompactSourceCodeRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#OCProgramNode #statementCharacter #false)) #'2023-01-02T00:23:08.164937+01:00') )
]

{ #category : 'meta data' }
ManifestASTCore class >> ruleRefersToClassRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#'ASTLiteralNode class' #literalToken: #true)) #'2013-03-29T18:06:27.069+01:00') )
]

{ #category : 'meta data' }
ManifestASTCore class >> ruleSendsDifferentSuperRuleV1TODO [
^ #(#(#(#RGMethodDefinition #(#OCMessageNode #replaceSourceWithMessageNode: #false)) #'2013-03-29T18:06:27.369+01:00') )
]
