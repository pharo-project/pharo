"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRefactoringChanges',
	#superclass : 'PackageManifest',
	#category : 'Refactoring-Changes-Manifest',
	#package : 'Refactoring-Changes',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestRefactoringChanges class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#RBRefactoryChangeFactory)) #'2021-10-08T23:52:53.51808+02:00') )
]
