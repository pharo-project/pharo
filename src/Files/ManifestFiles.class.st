Class {
	#name : #ManifestFiles,
	#superclass : #PackageManifest,
	#category : #'Files-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestFiles class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Zinc-Character-Encoding-Core' #'FileSystem-Core' #'Multilingual-TextConversion' #'Collections-Abstract' #'System-Platforms')
]

{ #category : #'code-critics' }
ManifestFiles class >> ruleRBClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'File class' #deleteFile: #true)) #'2018-05-14T09:42:47.857795-07:00') )
]
