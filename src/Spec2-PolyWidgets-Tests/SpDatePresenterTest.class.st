Class {
	#name : #SpDatePresenterTest,
	#superclass : #SpSmokeTestCase,
	#category : #'Spec2-PolyWidgets-Tests-Tests'
}

{ #category : #running }
SpDatePresenterTest >> classToTest [
	^ SpDatePresenter
]

{ #category : #tests }
SpDatePresenterTest >> testOnDateChosen [
	| mookChoseDate |
	mookChoseDate := ChoseDate new.
	mookChoseDate
		calendar: StandardWindow new;
		date: Date tomorrow.
	presenter onDateChosen: mookChoseDate.
	self assert: presenter date equals: Date tomorrow
]
