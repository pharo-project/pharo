"
Core package for the Spec UI framework
"
Class {
	#name : #ManifestSpecCore,
	#superclass : #PackageManifest,
	#category : #'Spec-Core-Manifest'
}

{ #category : #deprecation }
ManifestSpecCore class >> isDeprecated [
	"Spec should be replaced by Spec 2"

	^ true
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleEqualsTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ListPresenter #getSelectionStateFor: #false)) #'2016-07-01T15:56:13.455728+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleExcessiveMethodsRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ComposablePresenter)) #'2016-07-01T15:56:13.465539+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleExcessiveVariablesRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ComposablePresenter)) #'2016-07-01T15:56:13.372336+02:00') #(#(#RGClassDefinition #(#AbstractWidgetPresenter)) #'2016-07-01T15:56:13.372528+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleGTExampleNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ComposablePresenter)) #'2016-07-01T15:56:13.363445+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleImplementedNotSentRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'TreePresenter class' #exampleWithNoSpecifiedNodes #true)) #'2016-07-01T15:56:13.496495+02:00') #(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithoutOptions #true)) #'2016-07-01T15:56:13.496444+02:00') #(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithOptions #true)) #'2016-07-01T15:56:13.496455+02:00') #(#(#RGMethodDefinition #(#'TreePresenter class' #exampleWithCustomColumnsAndNodesAndChildren #true)) #'2016-07-01T15:56:13.496426+02:00') #(#(#RGMethodDefinition #(#'TreePresenter class' #exampleOfAutoRefreshOnExpand #true)) #'2016-07-01T15:56:13.496465+02:00') #(#(#RGMethodDefinition #(#'TreePresenter class' #exampleWithCustomColumnsAndNodes #true)) #'2016-07-01T15:56:13.496478+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#AbstractWidgetPresenter #initialize #false)) #'2016-07-01T15:56:13.356024+02:00') #(#(#RGMethodDefinition #(#ComposablePresenter #initialize #false)) #'2016-07-01T15:56:13.355982+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleRBEquivalentSuperclassMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ComposablePresenter class' #iconNamed: #true)) #'2018-11-07T14:51:01.186582+01:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleRBOverridesDeprecatedMethodRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#MenuItemPresenter #name #false)) #'2016-07-01T15:56:13.378417+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleSubclassResponsibilityNotDefinedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ComposablePresenter class' #defaultSpec #true)) #'2016-07-01T15:56:13.461086+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleUnwindBlocksRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'SpecInterpreter class' #interpretASpec:model:selector: #true)) #'2016-07-01T15:56:13.479538+02:00') )
]

{ #category : #'code-critics' }
ManifestSpecCore class >> ruleUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'DiffPresenter class' #exampleWithOptions #true)) #'2016-07-01T15:56:13.475288+02:00') )
]
