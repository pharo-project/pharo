Class {
	#name : #SpAbstractWidgetPresenterDeferringActionTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Core'
}

{ #category : #tests }
SpAbstractWidgetPresenterDeferringActionTest >> testWithAdapterPerformOrDeferDoesNotExecuteWhenNoAdapter [
	| presenter executed |
	
	presenter := SpAbstractWidgetPresenter new.
	executed := false.
	presenter withAdapterPerformOrDefer: [ :anAdapter | executed := true ].
	
	self deny: executed
]

{ #category : #tests }
SpAbstractWidgetPresenterDeferringActionTest >> testWithAdapterPerformOrDeferExecutesWhenAdapter [
	| presenter executed |
	
	presenter := SpAbstractWidgetPresenter new.
	presenter adapter: Object new.
	presenter withAdapterPerformOrDefer: [ :anAdapter | 
		executed := anAdapter. ].
	self assert: executed notNil
]
