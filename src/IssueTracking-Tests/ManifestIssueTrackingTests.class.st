"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestIssueTrackingTests,
	#superclass : #PackageManifest,
	#category : #'IssueTracking-Tests'
}

{ #category : #'code-critics' }
ManifestIssueTrackingTests >> ruleBadMessageRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#PharoIssueTest #testSlice #false)) #'2015-05-07T14:09:53.928371-04:00') )
]

{ #category : #'code-critics' }
ManifestIssueTrackingTests >> ruleVariableAssignedLiteralRuleV1FalsePositive [
^ #(#(#(#RGClassDefinition #(#PharoIssueTest)) #'2015-05-07T14:09:53.913685-04:00') )
]
