"
Tests for CalendarPresenter.
"
Class {
	#name : #SpCalendarPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'calendar'
	],
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpCalendarPresenterTest >> setUp [
	super setUp.
	calendar := SpCalendarPresenter new
]

{ #category : #tests }
SpCalendarPresenterTest >> testDayBeforeMonthOf [
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 9 day: 1)) equals: 6.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 9 day: 14)) equals: 6.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 9 day: 30)) equals: 6.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 10 day: 1)) equals: 1.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 10 day: 17)) equals: 1.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 10 day: 30)) equals: 1
]

{ #category : #tests }
SpCalendarPresenterTest >> testDaysAfterMonthOf [
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 9 day: 1)) equals: 6.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 9 day: 14)) equals: 6.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 9 day: 30)) equals: 6.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 10 day: 1)) equals: 10.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 10 day: 17)) equals: 10.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 10 day: 30)) equals: 10
]

{ #category : #tests }
SpCalendarPresenterTest >> testJulianDaysIntervalFor [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 15).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
SpCalendarPresenterTest >> testJulianDaysIntervalFor2 [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 17).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
SpCalendarPresenterTest >> testJulianDaysIntervalFor3 [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 1).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
SpCalendarPresenterTest >> testJulianDaysIntervalFor4 [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 30).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
SpCalendarPresenterTest >> testNextMonthButton [
	| d |
	d := (Date year: 2018 month: 9 day: 15).
	calendar adaptToDate: d.
	
	self assert: calendar monthYearLabel label equals: d month asString.
	calendar nextMonthButton performAction.
	
	self assert: calendar monthYearLabel label equals: (d addMonths: 1) month asString
]

{ #category : #tests }
SpCalendarPresenterTest >> testPreviousMonthButton [
	| d |
	d := (Date year: 2018 month: 9 day: 15).
	calendar adaptToDate: d.
	
	self assert: calendar monthYearLabel label equals: d month asString.
	calendar previousMonthButton performAction.
	
	self assert: calendar monthYearLabel label equals: (d addMonths: -1) month asString
]

{ #category : #tests }
SpCalendarPresenterTest >> testWhenDaySelectedBlock [
	| blockExecuted d |
	blockExecuted := false.
	d := Date year: 2018 month: 9 day: 15.
	calendar
		adaptToDate: d;
		whenDaySelectedBlock: [ blockExecuted := true ].
	self deny: blockExecuted.

	"Dirty hack to access the button."
	calendar daysButtons basicPresenters value values first performAction.
	self assert: blockExecuted
]
