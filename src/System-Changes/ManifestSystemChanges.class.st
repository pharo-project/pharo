"
Packacke with support for system changes recorded in changesets
"
Class {
	#name : 'ManifestSystemChanges',
	#superclass : 'PackageManifest',
	#category : 'System-Changes-Manifest',
	#package : 'System-Changes',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemChanges class >> manuallyResolvedDependencies [

	^ #( #'OpalCompiler-Core' #'FileSystem-Disk' #'Collections-Abstract' )
]

{ #category : 'code-critics' }
ManifestSystemChanges class >> ruleClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'ChangeSet class' #ChangeSet)) #'2021-10-04T20:06:46.530781+02:00') )
]
