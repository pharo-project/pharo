"
All weak collections
"
Class {
	#name : #ManifestCollectionsWeak,
	#superclass : #PackageManifest,
	#category : #'Collections-Weak-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsWeak class >> dependencies [
	^ #(#'System-Finalization' #'Collections-Support' #'Collections-Sequenceable' #'Collections-Unordered' #'Collections-Abstract' #Kernel)
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsWeak class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Collections-Streams')
]

{ #category : #'meta-data' }
ManifestCollectionsWeak class >> packageName [
	^ #'Collections-Weak'
]

{ #category : #'code-critics' }
ManifestCollectionsWeak class >> ruleNoUnusedInstanceVariableRuleV1FalsePositive [

	<ignoreForCoverage>
	"This rule is banned because the bootstrap cannot run without the third #flag instance variable in WeakSet and WeakIdentitySet"
	^ #(#(#(#RGPackageDefinition #(#'Collections-Weak')) #'2023-04-06T09:49:05.271404+02:00') )
]
