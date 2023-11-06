"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestFreeType',
	#superclass : 'PackageManifest',
	#category : 'FreeType-Manifest',
	#package : 'FreeType',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestFreeType class >> ruleLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'ManifestFreeType class' #ManifestFreeType)) #'2022-09-02T11:20:02.557122+02:00') )
]

{ #category : 'code-critics' }
ManifestFreeType class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FT2Face #loadFields #false)) #'2022-09-02T08:43:03.073154+02:00') #(#(#RGMethodDefinition #(#FreeTypeCache #atFont:charCode:type:ifAbsentPut: #false)) #'2022-09-02T09:26:55.843044+02:00') )
]

{ #category : 'code-critics' }
ManifestFreeType class >> ruleMissingSuperSendsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FT2Handle #initialize #false)) #'2022-09-02T08:32:16.460846+02:00') )
]

{ #category : 'code-critics' }
ManifestFreeType class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FreeTypeExternalMemory #primCopyToExternalMemory: #false)) #'2022-09-02T08:45:14.82839+02:00') )
]
