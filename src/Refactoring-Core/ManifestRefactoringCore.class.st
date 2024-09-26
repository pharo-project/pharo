"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRefactoringCore',
	#superclass : 'PackageManifest',
	#category : 'Refactoring-Core-Manifest',
	#package : 'Refactoring-Core',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestRefactoringCore class >> ruleNoUnusedInstanceVariableRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ReIsTemporaryVariableCondition)) #'2024-09-24T05:27:08.432121+02:00') )
]

{ #category : 'code-critics' }
ManifestRefactoringCore class >> ruleVariableReferencedOnceRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ReIsTemporaryVariableCondition)) #'2024-09-24T05:26:59.705114+02:00') )
]
