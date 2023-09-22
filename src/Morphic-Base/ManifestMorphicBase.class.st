"
Basic classes for Morphic - a direct-manipulation User Interface (UI) construction kit 
"
Class {
	#name : 'ManifestMorphicBase',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Base-Manifest',
	#package : 'Morphic-Base',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestMorphicBase class >> ruleRBClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'StringMorph class' #exampleManyStringMorphs #true)) #'2017-09-29T14:22:18.600353+02:00') )
]

{ #category : 'code-critics' }
ManifestMorphicBase class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'StringMorph class' #exampleManyStringMorphs #true)) #'2017-09-29T14:22:14.93688+02:00') )
]

{ #category : 'code-critics' }
ManifestMorphicBase class >> ruleRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'CircleMorph class' #example #true)) #'2021-11-23T21:02:44.097019+01:00') #(#(#RGMethodDefinition #(#'EllipseMorph class' #example #true)) #'2021-11-23T21:04:02.130492+01:00') #(#(#RGMethodDefinition #(#'CircleMorph class' #exampleBadExtent #true)) #'2021-11-23T21:05:37.844402+01:00') )
]
