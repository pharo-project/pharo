"
I am a composable presenter to show a calendar to the user.

See my example methods on class side.
"
Class {
	#name : #CalendarPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'previousMonthButton',
		'nextMonthButton',
		'monthYearLabel',
		'namesOfDaysLabels',
		'daysButtons',
		'whenDaySelectedBlock'
	],
	#category : #'Spec-PolyWidgets-Widgets'
}

{ #category : #specs }
CalendarPresenter class >> defaultSpec [

	^ SpecLayout composed
		newColumn: [ :mainColumn |
			mainColumn
				newRow: [ :topRow |
					topRow
						newColumn: #previousMonthButton;
						newColumn: #monthYearLabel;
						newColumn: #nextMonthButton ] height: self buttonHeight;
				newRow: #namesOfDaysLabels height: self buttonHeight;
				newRow: [ :daysRow |
					daysRow
						newColumn: #daysButtons ] ]
]

{ #category : #examples }
CalendarPresenter class >> exampleInspectClickedDate [
	CalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ :aDate | aDate inspect ];
		openWithSpec
]

{ #category : #examples }
CalendarPresenter class >> exampleInspectClickedDateAndClose [
	CalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ :aDate :calendarPresenter | aDate inspect. calendarPresenter window close ];
		openWithSpec
]

{ #category : #examples }
CalendarPresenter class >> exampleSimple [
	CalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ UIManager default inform: 'A date was clicked.' ];
		openWithSpec
]

{ #category : #adapting }
CalendarPresenter >> adaptToDate: aDate [
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
CalendarPresenter >> datesToDisplayFor: aDate [
	"Returns the dates to display in the calendar."
	^ (self julianDaysIntervalFor: aDate)
			collect: [ :julianNumber | Date julianDayNumber: julianNumber ]
]

{ #category : #private }
CalendarPresenter >> dayBeforeMonthOf: aDate [
	^ aDate month dates first dayOfWeek - 1
]

{ #category : #'private - event' }
CalendarPresenter >> dayClicked: aDate [
	self whenDaySelectedBlock cull: aDate cull: self
]

{ #category : #'private - constants' }
CalendarPresenter >> dayNames [
	^ #('Sun' 'Mon' 'Tue' 'Wed' 'Thu' 'Fri' 'Sat')
]

{ #category : #private }
CalendarPresenter >> daysAfterMonthOf: aDate [
	^ self daysToDisplayCount - aDate month daysInMonth - (self dayBeforeMonthOf: aDate)
]

{ #category : #accessing }
CalendarPresenter >> daysButtons [
	^ daysButtons
]

{ #category : #private }
CalendarPresenter >> daysButtons: anObject [
	daysButtons := anObject
]

{ #category : #private }
CalendarPresenter >> daysButtonsBuilderFrom: aDate [
	^ DynamicPresentersListBuilder new
		modelObjects: (self datesToDisplayFor: aDate);
		presenter: ButtonPresenter
		configuredAs: [ :button :date | 
			button
				label: date dayOfMonth asString;
				enabled: aDate monthIndex = date monthIndex;
				action: [ self dayClicked: date ].
			"Add icon next to current date."
			date julianDayNumber = Date today julianDayNumber
				ifTrue: [ button icon: (self iconNamed: #glamorousGrayCircle) ] ];
		layoutBuilder: (DynamicLeftToRightColumnsLayout columns: self daysInAWeek);
		yourself
]

{ #category : #'private - constants' }
CalendarPresenter >> daysInAWeek [
	"The number of days in a week."
	^ self dayNames size
]

{ #category : #private }
CalendarPresenter >> daysLabelsBuilder [
	^ DynamicPresentersListBuilder new
		modelObjects: self dayNames;
		presenter: LabelPresenter
			configuredAs: [ :label :str | label label: str ];
		layoutBuilder: (DynamicLeftToRightColumnsLayout columns: self daysInAWeek);
		yourself
]

{ #category : #'private - constants' }
CalendarPresenter >> daysToDisplayCount [
	^ self maxNumberOfWeeksToDisplay * self daysInAWeek
]

{ #category : #initialization }
CalendarPresenter >> initialize [
	super initialize.
	self
		title: 'Calendar' translated;
		whenDaySelectedBlock: [ :aDate | ]
]

{ #category : #initialization }
CalendarPresenter >> initializeWidgets [
	previousMonthButton := self newButton.
	previousMonthButton label: '<'.
	
	nextMonthButton := self newButton.
	nextMonthButton label: '>'.
	
	monthYearLabel := self newLabel.
	monthYearLabel label: '-'.
	
	namesOfDaysLabels := self newNullPresenter.
	
	daysButtons := self newNullPresenter
]

{ #category : #private }
CalendarPresenter >> julianDaysIntervalFor: aDate [
	"Returns an interval of integers with the first one being the first day to display in the UI and the last integer the last day.
	 The first and last days might not be in the same month as aDate provided as parameter."

	| start end |
	start := aDate julianDayNumber - aDate dayOfMonth - (self dayBeforeMonthOf: aDate) + 1.
	end := aDate julianDayNumber + (aDate month daysInMonth - aDate dayOfMonth) + (self daysAfterMonthOf: aDate).
	^ start to: end
]

{ #category : #'private - constants' }
CalendarPresenter >> maxNumberOfWeeksToDisplay [
	"Maximum number of weeks to display in the calendar for it to be always well displayed."
	^ 6
]

{ #category : #accessing }
CalendarPresenter >> monthYearLabel [
	^ monthYearLabel
]

{ #category : #accessing }
CalendarPresenter >> namesOfDaysLabels [
	^ namesOfDaysLabels
]

{ #category : #accessing }
CalendarPresenter >> namesOfDaysLabels: anObject [
	namesOfDaysLabels := anObject
]

{ #category : #accessing }
CalendarPresenter >> nextMonthButton [
	^ nextMonthButton
]

{ #category : #accessing }
CalendarPresenter >> previousMonthButton [
	^ previousMonthButton
]

{ #category : #accessing }
CalendarPresenter >> whenDaySelectedBlock [
	^ whenDaySelectedBlock
]

{ #category : #accessing }
CalendarPresenter >> whenDaySelectedBlock: aBlockWith0To2Arguments [
	"aBlockWith0To2Arguments provided as argument will be called when a day is clicked by the user.
	 If the block has 0 argument, the block is called as is when the user clicked a day.
	 If the block has 1 argument, the block is called with a Date corresponding to the day clicked.
	 If the block has 2 arguments, the block is called with a Date corresponding to the day clicked and myself as arguments."
	whenDaySelectedBlock := aBlockWith0To2Arguments
]
