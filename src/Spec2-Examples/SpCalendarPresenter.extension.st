Extension { #name : #SpCalendarPresenter }

{ #category : #'*Spec2-Examples' }
SpCalendarPresenter class >> exampleInspectClickedDate [
	SpCalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ :aDate | aDate inspect ];
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpCalendarPresenter class >> exampleInspectClickedDateAndClose [
	SpCalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ :aDate :calendarPresenter | aDate inspect. calendarPresenter window close ];
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpCalendarPresenter class >> exampleSimple [
	SpCalendarPresenter new
		adaptToDate: Date today;
		whenDaySelectedBlock: [ UIManager default inform: 'A date was clicked.' ];
		openWithSpec
]
