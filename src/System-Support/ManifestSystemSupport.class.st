"
General support for the Pharo system like access to the VM or image
"
Class {
	#name : #ManifestSystemSupport,
	#superclass : #PackageManifest,
	#category : #'System-Support-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSupport class >> ignoredDependencies [
	^ #(#'System-Settings-Core' #'Graphics-Primitives')
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSupport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Support' #'System-Settings-Core' #'System-Platforms')
]

{ #category : #'code-critics' }
ManifestSystemSupport class >> ruleClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'Author class' #useAuthor:during: #true)) #'2020-06-15T21:10:09.714675+02:00') )
]
