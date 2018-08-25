"
Utilities for issue tracking on Pharo development
"
Class {
	#name : #ManifestIssueTracking,
	#superclass : #PackageManifest,
	#category : #'IssueTracking-Manifest'
}

{ #category : #'code-critics' }
ManifestIssueTracking >> ruleMissingYourselfRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#PharoIssue #downloadTitle #false)) #'2015-05-07T14:07:56.041142-04:00') )
]
