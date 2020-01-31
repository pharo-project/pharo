"
A SpecInterpreterTest is a test class for testing the behavior of SpecInterpreter
"
Class {
	#name : #SpInterpreterTest,
	#superclass : #TestCase,
	#instVars : [
		'specInterpreter',
		'specInterpreterClass'
	],
	#category : #'Spec2-Tests-Utilities'
}

{ #category : #running }
SpInterpreterTest >> setUp [
	"Setting up code for SpecInterpreterTest"

	super setUp.
	specInterpreterClass := SpInterpreter.
	specInterpreter := specInterpreterClass new.
	specInterpreter presenter: SpTestingPresenter new
]

{ #category : #running }
SpInterpreterTest >> tearDown [
	"Tearing down code for SpecInterpreterTest"

	specInterpreterClass := nil.
	specInterpreter := nil.
	super tearDown 
]

{ #category : #tests }
SpInterpreterTest >> testConvertRandomSymbolOfClassToInstance [
	| symbol instance |
	symbol := #MenuRegistration.
	instance := specInterpreter convertSymbolOfClassToInstance: symbol.
	self assert: instance class name equals: symbol
]

{ #category : #tests }
SpInterpreterTest >> testDynamicBuild [
	| model widget1 widget2 widget3 |
	model := SpTestingPresenter new.
	model openWithSpec close.
	widget1 := model list adapter.
	model openWithSpec close.
	widget2 := model list adapter.
	self deny: widget1 identicalTo: widget2.
	model list needRebuild: false.
	model needRebuild: false.
	model openWithSpec close.
	widget3 := model list adapter.
	self assert: widget2 identicalTo: widget3.
	self assert: model needRebuild
]

{ #category : #tests }
SpInterpreterTest >> testInterpretASpecModelMorphAssociation [
	| spec model morph |
	model := SpAbstractWidgetPresenter new.
	spec := {#MenuRegistration . #help: . #icon:}.
	morph := specInterpreterClass interpretASpec: spec presenter: model.
	self assert: model adapter == morph
]

{ #category : #tests }
SpInterpreterTest >> testInterpreterCanAccessPresenterVariablesWithoutAccessor [
	| window |
	[ self shouldnt: [ window := SpMockPesenterWithoutGetter new openWithSpec ] raise: MessageNotUnderstood ]
		ensure: [ window ifNotNil: #close ]
]
