"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestASTCoreTests',
	#superclass : 'PackageManifest',
	#category : 'AST-Core-Tests-Manifest',
	#package : 'AST-Core-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestASTCoreTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Collections-Abstract')
]

{ #category : 'code-critics' }
ManifestASTCoreTests class >> ruleGRTemporaryNeitherReadNorWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASTEvaluationTest #testEvaluateForContext #false)) #'2019-07-05T11:16:20.959329+02:00') )
]
