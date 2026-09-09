"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRefactoringCriticsTests',
	#superclass : 'PackageManifest',
	#category : 'Refactoring-Critics-Tests-Manifest',
	#package : 'Refactoring-Critics-Tests',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestRefactoringCriticsTests class >> isDeprecated [ 

	^ true
]

{ #category : 'code-critics' }
ManifestRefactoringCriticsTests class >> ruleReCompactSourceCodeRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#RBSmalllintTestObject #multiplePeriodsTerminatingStatement #false)) #'2023-02-08T19:04:24.544321+01:00') #(#(#RGMethodDefinition #(#RBSmalllintTestObject #multiplePeriodsTerminatingAssignmentStatement #false)) #'2023-02-08T19:04:34.298399+01:00') )
]
