"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestUndefinedClassesTests',
	#superclass : 'PackageManifest',
	#category : 'UndefinedClasses-Tests-Manifest',
	#package : 'UndefinedClasses-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestUndefinedClassesTests class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#UndefinedClassTest #createClassFromDefinitionString: #false)) #'2021-11-17T07:23:16.420579+01:00') )
]
