"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestFluidClassBuilder',
	#superclass : 'PackageManifest',
	#category : 'FluidClassBuilder-Manifest',
	#package : 'FluidClassBuilder',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestFluidClassBuilder class >> ruleLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'ManifestFluidClassBuilder class' #ManifestFluidClassBuilder)) #'2022-08-31T22:15:04.019152+02:00') )
]

{ #category : 'code-critics' }
ManifestFluidClassBuilder class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FluidClassSideTraitBuilder #slots: #false)) #'2022-08-31T22:05:04.078571+02:00') #(#(#RGMethodDefinition #(#FluidClassSideClassBuilder #slots: #false)) #'2022-08-31T22:05:15.962103+02:00') #(#(#RGMethodDefinition #(#UndefinedObject #'<<' #false)) #'2022-08-31T22:14:53.135206+02:00') #(#(#RGMethodDefinition #(#FluidClassSideTraitBuilder #traits: #false)) #'2022-08-31T22:18:41.545354+02:00') #(#(#RGMethodDefinition #(#FluidClassSideClassBuilder #traits: #false)) #'2022-08-31T22:18:47.62601+02:00') )
]
