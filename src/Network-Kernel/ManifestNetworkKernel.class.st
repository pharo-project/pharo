"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestNetworkKernel,
	#superclass : #PackageManifest,
	#category : #'Network-Kernel-Manifest'
}

{ #category : #'code-critics' }
ManifestNetworkKernel class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'HTTPProgress class' #example #true)) #'2019-07-05T10:28:44.466329+02:00') )
]
