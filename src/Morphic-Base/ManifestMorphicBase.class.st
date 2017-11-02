"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestMorphicBase,
	#superclass : #PackageManifest,
	#category : 'Morphic-Base'
}

{ #category : #code-critics }
ManifestMorphicBase class >> ruleRBClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'StringMorph class' #exampleManyStringMorphs #true)) #'2017-09-29T14:22:18.600353+02:00') )
]

{ #category : #code-critics }
ManifestMorphicBase class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'StringMorph class' #exampleManyStringMorphs #true)) #'2017-09-29T14:22:14.93688+02:00') )
]
