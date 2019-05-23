"
A SpecAbstractWidgetLayoutTest is a test class for testing the behavior of SpecAbstractWidgetLayout
"
Class {
	#name : #SpecAbstractWidgetLayoutTest,
	#superclass : #TestCase,
	#category : #'Spec-Tests-Layout'
}

{ #category : #test }
SpecAbstractWidgetLayoutTest >> testAdapterForBindings [
	| layout |
	layout := SpecAbstractWidgetLayout for: #ListAdapter.
	self assert: (layout adapterFor: ListPresenter new bindings: SpecStubAdapterBindings new) class equals: SpecStubListAdapter
]

{ #category : #test }
SpecAbstractWidgetLayoutTest >> testAdapterForBindingsRaiseErrorIfNoBinding [
	| layout |
	layout := SpecAbstractWidgetLayout for: #NonExistingAdapter.
	self should: [ layout adapterFor: ListPresenter new bindings: SpecStubAdapterBindings new ] raise: Error
]

{ #category : #test }
SpecAbstractWidgetLayoutTest >> testBuildAdapterForBindings [
	| layout |
	layout := SpecAbstractWidgetLayout for: #ListAdapter.
	self assert: (layout buildAdapterFor: ListPresenter new bindings: SpecStubAdapterBindings new) widget class equals: SpecStubListView
]

{ #category : #test }
SpecAbstractWidgetLayoutTest >> testDynamicBuild [
	"Cyril: This is currently a duplicated of a test in SpecInterpreter, but this test is useful for more that the SpecInterpreter and since we plan to remove the interpreter I want to ensure we do not lose this test. This comment can be removed once the SpecInterpreter will be removed."

	| model widget1 widget2 widget3 |
	model := TestingComposablePresenter new.
	model openWithSpec close.
	widget1 := model list adapter.
	model openWithSpec close.
	widget2 := model list adapter.
	self deny: widget1 == widget2.
	model list needRebuild: false.
	model needRebuild: false.
	model openWithSpec close.
	widget3 := model list adapter.
	self assert: widget2 == widget3.
	self assert: model needRebuild
]
