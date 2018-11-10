"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestIceberg,
	#superclass : #PackageManifest,
	#category : #'Iceberg-Manifest'
}

{ #category : #'meta-data' }
ManifestIceberg class >> description [ ^ 'Iceberg is a set of tools that allow one to handle git repositories directly from a Pharo image. Right now we support only git, but Iceberg is designed to allow other code versioning systems in the future. The final aim of Iceberg is to become the default repository manager for Pharo-core, allowing for smoother and faster integration of contributions, as well as better branch and version management.

Repository : https://github.com/pharo-vcs/iceberg

This package contains the core classes of Iceberg.'
]

{ #category : #'code-critics' }
ManifestIceberg class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'IceRepository class' #isAbstract #true)) #'2018-03-13T18:40:58.956177+01:00') )
]

{ #category : #'code-critics' }
ManifestIceberg class >> ruleRBTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#IceRepository #outgoingCommits #false)) #'2016-06-16T17:24:18.524965+02:00') #(#(#RGMethodDefinition #(#IceMCGitRepository #outgoingCommits #false)) #'2016-07-06T15:14:46.350843+02:00') )
]

{ #category : #'code-critics' }
ManifestIceberg class >> ruleRBUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#IceRepository #fetchFrom: #false)) #'2018-03-13T16:34:52.218313+01:00') )
]
