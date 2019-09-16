"
A SpecAbstractWidgetLayoutTest is a test class for testing the behavior of SpecAbstractWidgetLayout
"
Class {
	#name : #SpAbstractWidgetLayoutTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #test }
SpAbstractWidgetLayoutTest >> testAdapterForBindings [
	| layout |
	layout := SpAbstractWidgetLayout for: #ListAdapter.
	self assert: (layout adapterFor: SpListPresenter new bindings: SpStubAdapterBindings new) class equals: SpStubListAdapter
]

{ #category : #test }
SpAbstractWidgetLayoutTest >> testAdapterForBindingsRaiseErrorIfNoBinding [
	| layout |
	layout := SpAbstractWidgetLayout for: #NonExistingAdapter.
	self should: [ layout adapterFor: SpListPresenter new bindings: SpStubAdapterBindings new ] raise: Error
]

{ #category : #test }
SpAbstractWidgetLayoutTest >> testBuildAdapterForBindings [
	| layout |
	layout := SpAbstractWidgetLayout for: #ListAdapter.
	self assert: (layout buildAdapterFor: SpListPresenter new bindings: SpStubAdapterBindings new) widget class equals: SpStubListView
]

{ #category : #test }
SpAbstractWidgetLayoutTest >> testDynamicBuild [
	"Cyril: This is currently a duplicated of a test in SpecInterpreter, but this test is useful for more that the SpecInterpreter and since we plan to remove the interpreter I want to ensure we do not lose this test. This comment can be removed once the SpecInterpreter will be removed."

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
