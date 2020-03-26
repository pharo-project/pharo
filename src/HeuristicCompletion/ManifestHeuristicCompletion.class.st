"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestHeuristicCompletion,
	#superclass : #PackageManifest,
	#category : #'HeuristicCompletion-Manifest'
}

{ #category : #'code-critics' }
ManifestHeuristicCompletion class >> ruleReInspectorPresentationsNonlazyCodeV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#CoStaticBenchmarks #gtResultsTableIn: #false)) #'2020-03-26T14:49:28.1129+01:00') )
]

{ #category : #'code-critics' }
ManifestHeuristicCompletion class >> ruleReSendsDifferentSuperRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'CoSystemEnvironment class' #forSmalltalkGlobals #true)) #'2020-03-26T14:29:31.095031+01:00') )
]

{ #category : #'code-critics' }
ManifestHeuristicCompletion class >> ruleReTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#CoFetcher #next: #false)) #'2020-03-26T14:50:53.739628+01:00') #(#(#RGMethodDefinition #(#CoTypeInferencer #visitCascadeNode: #false)) #'2020-03-26T14:50:59.173856+01:00') )
]

{ #category : #'code-critics' }
ManifestHeuristicCompletion class >> ruleReUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#CoTypeInferencer #visitMessageNode:receiverType: #false)) #'2020-03-26T14:21:20.676631+01:00') )
]
