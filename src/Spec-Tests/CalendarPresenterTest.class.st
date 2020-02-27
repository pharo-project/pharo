"
Tests for CalendarPresenter.
"
Class {
	#name : #CalendarPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'calendar'
	],
	#category : #'Spec-Tests-PolyWidgets-Widgets'
}

{ #category : #running }
CalendarPresenterTest >> setUp [
	super setUp.
	calendar := CalendarPresenter new
]

{ #category : #tests }
CalendarPresenterTest >> testDayBeforeMonthOf [
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 9 day: 1)) equals: 6.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 9 day: 14)) equals: 6.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 9 day: 30)) equals: 6.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 10 day: 1)) equals: 1.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 10 day: 17)) equals: 1.
	self assert: (calendar dayBeforeMonthOf: (Date year: 2018 month: 10 day: 30)) equals: 1
]

{ #category : #tests }
CalendarPresenterTest >> testDaysAfterMonthOf [
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 9 day: 1)) equals: 6.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 9 day: 14)) equals: 6.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 9 day: 30)) equals: 6.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 10 day: 1)) equals: 10.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 10 day: 17)) equals: 10.
	self assert: (calendar daysAfterMonthOf: (Date year: 2018 month: 10 day: 30)) equals: 10
]

{ #category : #tests }
CalendarPresenterTest >> testJulianDaysIntervalFor [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 15).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
CalendarPresenterTest >> testJulianDaysIntervalFor2 [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 17).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
CalendarPresenterTest >> testJulianDaysIntervalFor3 [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 1).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
CalendarPresenterTest >> testJulianDaysIntervalFor4 [
	| interval |
	interval := calendar julianDaysIntervalFor: (Date year: 2018 month: 9 day: 30).
	
	self
		assert: interval size equals: calendar daysToDisplayCount;
		assert: interval
		equals: ((Date year: 2018 month: 8 day: 26) julianDayNumber to: (Date year: 2018 month: 10 day: 6) julianDayNumber)
]

{ #category : #tests }
CalendarPresenterTest >> testNextMonthButton [
	| d |
	d := (Date year: 2018 month: 9 day: 15).
	calendar adaptToDate: d.
	
	self assert: calendar monthYearLabel label equals: d month asString.
	calendar nextMonthButton performAction.
	
	self assert: calendar monthYearLabel label equals: (d addMonths: 1) month asString
]

{ #category : #tests }
CalendarPresenterTest >> testPreviousMonthButton [
	| d |
	d := (Date year: 2018 month: 9 day: 15).
	calendar adaptToDate: d.
	
	self assert: calendar monthYearLabel label equals: d month asString.
	calendar previousMonthButton performAction.
	
	self assert: calendar monthYearLabel label equals: (d addMonths: -1) month asString
]

{ #category : #tests }
CalendarPresenterTest >> testWhenDaySelectedBlock [
	| blockExecuted d |
	blockExecuted := false.
	d := (Date year: 2018 month: 9 day: 15).
	calendar
		adaptToDate: d;
		whenDaySelectedBlock: [ blockExecuted := true ].
		
	self deny: blockExecuted.
	
	"Dirty hack to access the button."
	calendar daysButtons widgets value values first performAction.
	
	self assert: blockExecuted
]
