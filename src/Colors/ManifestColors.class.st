"
Support for colors and color maps
"
Class {
	#name : 'ManifestColors',
	#superclass : 'PackageManifest',
	#category : 'Colors-Manifest',
	#package : 'Colors',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestColors class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : 'code-critics' }
ManifestColors class >> ruleClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'Color class' #Color)) #'2021-10-04T22:19:47.752887+02:00') )
]
