"
I am a package part of the Spec 2 project.

Spec is a framework in Pharo for describing user interfaces. It allows for the construction of a wide variety of UIs; from small windows with a few but- tons up to complex tools like a debugger. Indeed, multiple tools in Pharo are written in Spec, e.g., Iceberg, Change Sorter, Critics Browser, and the Pharo debugger.

I contain the core of Spec 2. By core I mean all the code defining the basics of Spec and the basic widgets users should use to build their applications.
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
ManifestSpec2Core class >> ruleRBOverridesDeprecatedMethodRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#SpMenuItemPresenter #name #false)) #'2016-07-01T15:56:13.378417+02:00') )
]

{ #category : #'code-critics' }
ManifestSpec2Core class >> ruleUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithOptions #true)) #'2016-07-01T15:56:13.475288+02:00') )
]
