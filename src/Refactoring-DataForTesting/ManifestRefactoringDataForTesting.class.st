"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRefactoringDataForTesting',
	#superclass : 'PackageManifest',
	#category : 'Refactoring-DataForTesting-Manifest',
	#package : 'Refactoring-DataForTesting',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestRefactoringDataForTesting class >> ruleReIvarNeitherReadNorWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ReClassForGeneratingPrintOn)) #'2024-06-20T19:43:13.245998+02:00') #(#(#RGClassDefinition #(#ReClassForGeneratingEqualAndHashExistingImplementors)) #'2024-06-20T19:43:13.245838+02:00') #(#(#RGClassDefinition #(#ReClassForGeneratingEqualAndHash)) #'2024-06-20T19:43:13.245672+02:00') )
]
