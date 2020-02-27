"
A SpecInterpreterTest is a test class for testing the behavior of SpecInterpreter
"
Class {
	#name : #SpecInterpreterTest,
	#superclass : #TestCase,
	#instVars : [
		'specInterpreter',
		'specInterpreterClass'
	],
	#category : #'Spec-Tests-Utilities'
}

{ #category : #running }
SpecInterpreterTest >> setUp [
	"Setting up code for SpecInterpreterTest"

	super setUp.
	specInterpreterClass := SpecInterpreter.
	specInterpreter := specInterpreterClass new.
	specInterpreter model: TestingComposablePresenter new
]

{ #category : #running }
SpecInterpreterTest >> tearDown [
	"Tearing down code for SpecInterpreterTest"

	specInterpreterClass := nil.
	specInterpreter := nil.
	super tearDown 
]

{ #category : #tests }
SpecInterpreterTest >> testBuildWidgetForWithSpec [
	| model morph |
	model := TestingComposablePresenter new.
	morph := model buildWithSpec: #testingSpec.
	self assert: morph model identicalTo: model.
	self assert: model widget class equals: RubPluggableTextMorph
]

{ #category : #tests }
SpecInterpreterTest >> testConvertRandomSymbolOfClassToInstance [
	| symbol instance |
	symbol := #PluggableListMorph.

	instance := specInterpreter convertSymbolOfClassToInstance: symbol.
	self assert: instance class name equals: symbol
]

{ #category : #tests }
SpecInterpreterTest >> testDynamicBuild [
	| model widget1 widget2 widget3 |
	model := TestingComposablePresenter new.
	model openWithSpec close.
	widget1 := model list widget.

	model openWithSpec close.
	widget2 := model list widget.

	self deny: widget1 identicalTo: widget2.

	model list needRebuild: false.
	model needRebuild: false.
	model openWithSpec close.
	widget3 := model list widget.

	self assert: widget2 identicalTo: widget3.
	self assert: model needRebuild
]

{ #category : #tests }
SpecInterpreterTest >> testInterpretASpecModelMorphAssociation [
	| spec model morph |
	model := AbstractWidgetPresenter new.
	spec := {#PluggableListMorph . #model: . #model}.
	morph := specInterpreterClass interpretASpec: spec model: model.

	self assert: model widget identicalTo: morph
]
