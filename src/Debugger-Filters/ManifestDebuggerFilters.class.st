"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestDebuggerFilters,
	#superclass : #PackageManifest,
	#category : #'Debugger-Filters-Manifest'
}

{ #category : #'code-critics' }
ManifestDebuggerFilters class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#BooleanFilter #shouldDisplay: #false)) #'2020-06-24T08:38:44.359688+02:00') )
]

{ #category : #'code-critics' }
ManifestDebuggerFilters class >> ruleCollectionMessagesToExternalObjectRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'SelectorFilter class' #SelectorFilter)) #'2020-06-24T08:39:44.785251+02:00') )
]

{ #category : #'code-critics' }
ManifestDebuggerFilters class >> ruleUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#KernelClassesFilter #kernelClassesToExclude #false)) #'2020-06-24T08:38:55.095241+02:00') )
]
