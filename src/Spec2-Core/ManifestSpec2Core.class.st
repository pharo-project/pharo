"
Core package for the Spec UI framework
"
Class {
	#name : #ManifestSpec2Core,
	#superclass : #PackageManifest,
	#category : #'Spec2-Core-Manifest'
}

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleExcessiveMethodsRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#SpPresenter)) #'2016-07-01T15:56:13.465539+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleExcessiveVariablesRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#SpPresenter)) #'2016-07-01T15:56:13.372336+02:00') #(#(#RGClassDefinition #(#SpAbstractWidgetPresenter)) #'2016-07-01T15:56:13.372528+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleGTExampleNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#SpPresenter)) #'2016-07-01T15:56:13.363445+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleImplementedNotSentRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'TreePresenter class' #exampleWithNoSpecifiedNodes #true)) #'2016-07-01T15:56:13.496495+02:00') #(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithoutOptions #true)) #'2016-07-01T15:56:13.496444+02:00') #(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithOptions #true)) #'2016-07-01T15:56:13.496455+02:00') #(#(#RGMethodDefinition #(#'TreePresenter class' #exampleWithCustomColumnsAndNodesAndChildren #true)) #'2016-07-01T15:56:13.496426+02:00') #(#(#RGMethodDefinition #(#'TreePresenter class' #exampleOfAutoRefreshOnExpand #true)) #'2016-07-01T15:56:13.496465+02:00') #(#(#RGMethodDefinition #(#'TreePresenter class' #exampleWithCustomColumnsAndNodes #true)) #'2016-07-01T15:56:13.496478+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SpAbstractWidgetPresenter #initialize #false)) #'2016-07-01T15:56:13.356024+02:00') #(#(#RGMethodDefinition #(#SpPresenter #initialize #false)) #'2016-07-01T15:56:13.355982+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleRBEquivalentSuperclassMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ComposablePresenter class' #iconNamed: #true)) #'2018-11-07T10:48:10.451257+01:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleRBOverridesDeprecatedMethodRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SpMenuItemPresenter #name #false)) #'2016-07-01T15:56:13.378417+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleSubclassResponsibilityNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ComposablePresenter class' #defaultSpec #true)) #'2016-07-01T15:56:13.461086+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithOptions #true)) #'2016-07-01T15:56:13.475288+02:00') )
]
