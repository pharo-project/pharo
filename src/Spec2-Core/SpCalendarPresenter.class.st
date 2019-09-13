"
I am a composable presenter to show a calendar to the user.

See my example methods on class side.
"
Class {
	#name : #SpCalendarPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'previousMonthButton',
		'nextMonthButton',
		'monthYearLabel',
		'namesOfDaysLabels',
		'daysButtons',
		'whenDaySelectedBlock'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpCalendarPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: #previousMonthButton;
				add: #monthYearLabel;
				add: #nextMonthButton)
			withConstraints: [ :constraints | constraints height: self buttonHeight ];
		add: #namesOfDaysLabels withConstraints: [ :constraints | constraints height: self buttonHeight ];
		add: #daysButtons;
		yourself
]

{ #category : #adapting }
SpCalendarPresenter >> adaptToDate: aDate [
	self monthYearLabel label: aDate month asString.
	self previousMonthButton
		action: [ self adaptToDate: aDate onPreviousMonth ].
	self nextMonthButton
		action: [ self adaptToDate: aDate onNextMonth ].
	self
		newDynamicPresentersListIn: #namesOfDaysLabels
		usingBuilder: self daysLabelsBuilder.
	self
		newDynamicPresentersListIn: #daysButtons
		usingBuilder: (self daysButtonsBuilderFrom: aDate)
]

{ #category : #private }
SpCalendarPresenter >> datesToDisplayFor: aDate [
	"Returns the dates to display in the calendar."
	^ (self julianDaysIntervalFor: aDate)
			collect: [ :julianNumber | Date julianDayNumber: julianNumber ]
]

{ #category : #private }
SpCalendarPresenter >> dayBeforeMonthOf: aDate [
	^ aDate month dates first dayOfWeek - 1
]

{ #category : #'private - event' }
SpCalendarPresenter >> dayClicked: aDate [
	self whenDaySelectedBlock cull: aDate cull: self
]

{ #category : #'private - constants' }
SpCalendarPresenter >> dayNames [
	^ #('Sun' 'Mon' 'Tue' 'Wed' 'Thu' 'Fri' 'Sat')
]

{ #category : #private }
SpCalendarPresenter >> daysAfterMonthOf: aDate [
	^ self daysToDisplayCount - aDate month daysInMonth - (self dayBeforeMonthOf: aDate)
]

{ #category : #accessing }
SpCalendarPresenter >> daysButtons [
	^ daysButtons
]

{ #category : #private }
SpCalendarPresenter >> daysButtons: anObject [
	daysButtons := anObject
]

{ #category : #private }
SpCalendarPresenter >> daysButtonsBuilderFrom: aDate [
	^ SpDynamicPresentersListBuilder new
		modelObjects: (self datesToDisplayFor: aDate);
		presenter: SpButtonPresenter
		configuredAs: [ :button :date | 
			button
				label: date dayOfMonth asString;
				enabled: aDate monthIndex = date monthIndex;
				action: [ self dayClicked: date ].
			"Add icon next to current date."
			date julianDayNumber = Date today julianDayNumber
				ifTrue: [ button icon: (self iconNamed: #glamorousGrayCircle) ] ];
		layoutBuilder: (SpDynamicLeftToRightColumnsLayout columns: self daysInAWeek);
		yourself
]

{ #category : #'private - constants' }
SpCalendarPresenter >> daysInAWeek [
	"The number of days in a week."
	^ self dayNames size
]

{ #category : #private }
SpCalendarPresenter >> daysLabelsBuilder [
	^ SpDynamicPresentersListBuilder new
		modelObjects: self dayNames;
		presenter: SpLabelPresenter
			configuredAs: [ :label :str | label label: str ];
		layoutBuilder: (SpDynamicLeftToRightColumnsLayout columns: self daysInAWeek);
		yourself
]

{ #category : #'private - constants' }
SpCalendarPresenter >> daysToDisplayCount [
	^ self maxNumberOfWeeksToDisplay * self daysInAWeek
]

{ #category : #initialization }
SpCalendarPresenter >> initialize [
	super initialize.
	self whenDaySelectedBlock: [ :aDate |  ]
]

{ #category : #initialization }
SpCalendarPresenter >> initializePresenters [
	previousMonthButton := self newButton.
	previousMonthButton label: '<'.
	
	nextMonthButton := self newButton.
	nextMonthButton label: '>'.
	
	monthYearLabel := self newLabel.
	monthYearLabel label: '-'.
	
	namesOfDaysLabels := self newNullPresenter.
	
	daysButtons := self newNullPresenter
]

{ #category : #initialization }
SpCalendarPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter title: 'Calendar'
]

{ #category : #private }
SpCalendarPresenter >> julianDaysIntervalFor: aDate [
	"Returns an interval of integers with the first one being the first day to display in the UI and the last integer the last day.
	 The first and last days might not be in the same month as aDate provided as parameter."

	| start end |
	start := aDate julianDayNumber - aDate dayOfMonth - (self dayBeforeMonthOf: aDate) + 1.
	end := aDate julianDayNumber + (aDate month daysInMonth - aDate dayOfMonth) + (self daysAfterMonthOf: aDate).
	^ start to: end
]

{ #category : #'private - constants' }
SpCalendarPresenter >> maxNumberOfWeeksToDisplay [
	"Maximum number of weeks to display in the calendar for it to be always well displayed."
	^ 6
]

{ #category : #accessing }
SpCalendarPresenter >> monthYearLabel [
	^ monthYearLabel
]

{ #category : #accessing }
SpCalendarPresenter >> namesOfDaysLabels [
	^ namesOfDaysLabels
]

{ #category : #accessing }
SpCalendarPresenter >> namesOfDaysLabels: anObject [
	namesOfDaysLabels := anObject
]

{ #category : #accessing }
SpCalendarPresenter >> nextMonthButton [
	^ nextMonthButton
]

{ #category : #accessing }
SpCalendarPresenter >> previousMonthButton [
	^ previousMonthButton
]

{ #category : #accessing }
SpCalendarPresenter >> whenDaySelectedBlock [
	^ whenDaySelectedBlock
]

{ #category : #accessing }
SpCalendarPresenter >> whenDaySelectedBlock: aBlockWith0To2Arguments [
	"aBlockWith0To2Arguments provided as argument will be called when a day is clicked by the user.
	 If the block has 0 argument, the block is called as is when the user clicked a day.
	 If the block has 1 argument, the block is called with a Date corresponding to the day clicked.
	 If the block has 2 arguments, the block is called with a Date corresponding to the day clicked and myself as arguments."
	whenDaySelectedBlock := aBlockWith0To2Arguments
]
