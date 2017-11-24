Class {
	#name : #DatePresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-PolyWidgets-Widgets'
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
	testedInstance onDateChosen: mookChoseDate.
	self assert: testedInstance date equals: Date tomorrow
]
