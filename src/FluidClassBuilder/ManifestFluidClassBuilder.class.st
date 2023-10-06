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

	^ #( #( #( #RGMethodDefinition #( #UndefinedObject #'<<' #false ) ) #'2022-08-31T22:14:53.135206+02:00' ) )
]
