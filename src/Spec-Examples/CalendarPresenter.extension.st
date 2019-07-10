Extension { #name : #CalendarPresenter }

{ #category : #'*Spec-Examples' }
CalendarPresenter class >> exampleInspectClickedDate [
	CalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ :aDate | aDate inspect ];
		openWithSpec
]

{ #category : #'*Spec-Examples' }
CalendarPresenter class >> exampleInspectClickedDateAndClose [
	CalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ :aDate :calendarPresenter | aDate inspect. calendarPresenter window close ];
		openWithSpec
]

{ #category : #'*Spec-Examples' }
CalendarPresenter class >> exampleSimple [
	CalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ UIManager default inform: 'A date was clicked.' ];
		openWithSpec
]
