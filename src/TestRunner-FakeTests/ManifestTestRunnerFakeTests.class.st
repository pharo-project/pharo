"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestTestRunnerFakeTests',
	#superclass : 'PackageManifest',
	#category : 'TestRunner-FakeTests-Manifest',
	#package : 'TestRunner-FakeTests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestTestRunnerFakeTests class >> ruleReTestClassNameShouldEndWithTestRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'TestRunner-FakeTests')) #'2026-03-12T08:30:13.747425-03:00') )
]

{ #category : 'code-critics' }
ManifestTestRunnerFakeTests class >> ruleReTestClassNotInPackageWithTestEndingNameRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'TestRunner-FakeTests')) #'2026-03-12T08:28:56.081057-03:00') )
]
