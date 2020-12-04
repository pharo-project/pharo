Class {
	#name : #SpDatePresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Morphic-Tests'
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
