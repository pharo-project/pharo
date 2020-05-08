"
Package for compression utilities like ZIP and other
"
Class {
	#name : #ManifestCompression,
	#superclass : #PackageManifest,
	#category : #'Compression-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestCompression class >> manuallyResolvedDependencies [
	^ #(#Jobs #'System-Support' #'System-Changes' #'Collections-Abstract')

]

{ #category : #'code-critics' }
ManifestCompression class >> ruleClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ZipArchive class' #isZipArchive: #true)) #'2020-04-30T09:18:38.202597+02:00') )
]
