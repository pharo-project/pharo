"
Basic classes for Morphic - a direct-manipulation User Interface (UI) construction kit 
"
Class {
	#name : 'ManifestMorphicWidgets',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Widgets-Manifest',
	#package : 'Morphic-Widgets',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMorphicWidgets class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #()
]

{ #category : 'code-critics' }
ManifestMorphicWidgets class >> ruleRBClassNameInSelectorRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'StringMorph class' #exampleManyStringMorphs #true)) #'2017-09-29T14:22:18.600353+02:00') )
]

{ #category : 'code-critics' }
ManifestMorphicWidgets class >> ruleRBRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'StringMorph class' #exampleManyStringMorphs #true)) #'2017-09-29T14:22:14.93688+02:00') )
]

{ #category : 'code-critics' }
ManifestMorphicWidgets class >> ruleRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'CircleMorph class' #example #true)) #'2021-11-23T21:02:44.097019+01:00') #(#(#RGMethodDefinition #(#'EllipseMorph class' #example #true)) #'2021-11-23T21:04:02.130492+01:00') #(#(#RGMethodDefinition #(#'CircleMorph class' #exampleBadExtent #true)) #'2021-11-23T21:05:37.844402+01:00') )
]
