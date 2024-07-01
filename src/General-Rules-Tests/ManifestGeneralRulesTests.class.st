"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestGeneralRulesTests',
	#superclass : 'PackageManifest',
	#category : 'General-Rules-Tests-Manifest',
	#package : 'General-Rules-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestGeneralRulesTests class >> ruleMethodSelectorKeywordCasingRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ReMethodSelectorKeywordCasingRuleTest #keyWordMethodWith:WrongCasingAtEnd: #false)) #'2024-02-19T21:57:16.289796+01:00') #(#(#RGMethodDefinition #(#ReMethodSelectorKeywordCasingRuleTest #KeyWordMethodWith:wrongCasingAtEnd: #false)) #'2024-02-19T21:57:21.447237+01:00') )
]
