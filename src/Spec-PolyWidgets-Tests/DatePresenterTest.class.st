Class {
	#name : #DatePresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-PolyWidgets-Tests'
}

{ #category : #running }
DatePresenterTest >> classToTest [
	^ DatePresenter
]

{ #category : #tests }
DatePresenterTest >> testOnDateChosen [
	| mookChoseDate |
	mookChoseDate := ChoseDate new.
	mookChoseDate
		calendar: StandardWindow new;
		date: Date tomorrow.
	presenter onDateChosen: mookChoseDate.
	self assert: presenter date equals: Date tomorrow
]
