"
Includes file and directory handling as well as file streams
"
Class {
	#name : 'ManifestFiles',
	#superclass : 'PackageManifest',
	#category : 'Files-Manifest',
	#package : 'Files',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestFiles class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #( #'Collections-Abstract' #'System-Platforms' #'System-Support' )
]

{ #category : 'code-critics' }
ManifestFiles class >> ruleRBClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'File class' #deleteFile: #true)) #'2018-05-14T09:42:47.857795-07:00') )
]
