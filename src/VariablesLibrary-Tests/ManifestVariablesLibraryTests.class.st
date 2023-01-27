"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestVariablesLibraryTests,
	#superclass : #PackageManifest,
	#category : #'VariablesLibrary-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestVariablesLibraryTests class >> ruleSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ComputedSlotTest #testReadComputedSlotCompiled #false)) #'2022-05-24T13:00:35.901695+02:00') )
]
