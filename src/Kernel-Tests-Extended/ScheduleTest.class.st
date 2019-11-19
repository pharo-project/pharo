"
SUnit tests for recurring schedules (class Schedule).
"
Class {
	#name : #ScheduleTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'firstEvent',
		'aSchedule',
		'restoredTimeZone'
	],
	#category : #'Kernel-Tests-Extended-Chronology'
}

{ #category : #coverage }
ScheduleTest >> classToBeTested [

	^ Schedule


]

{ #category : #coverage }
ScheduleTest >> selectorsToBeIgnored [

	| private | 
	private := #( #printOn: ).

	^ super selectorsToBeIgnored, private

]

{ #category : #running }
ScheduleTest >> setUp [
 	 "Schedule is a type of Timespan representing repeated occurences of the same event. 
	The beginning of the schedule is the first occurrence of the event.
	A schedule maintains an array of Durations. 
	Each durations specify the offset to the next scheduled each. 
	The duration of each occurence of the event is not specified. 
	Nor are any other attributes such as name"

	super setUp.
	restoredTimeZone := DateAndTime localTimeZone.
	DateAndTime localTimeZone: (TimeZone timeZones detect: [:tz | tz abbreviation = 'GMT']).

	"Create aSchedule with an event scheduled for 8:30pm every Saturday 
	and Sunday for the year 2003. "
	"Create the first event occurring on the first Saturday at 8:30 pm: 1/4/03"
	firstEvent :=  DateAndTime year: 2003 month: 1 day: 4 hour: 20 minute: 30. 
	
	"Create a schedule for one year starting with the first event" 
	aSchedule := Schedule starting: firstEvent duration: 52 weeks.

	"Schedule the recurring events by scheduling the time in between each one.  
	One day for Sat-Sun. 6 days for Sun-Sat" 
	aSchedule schedule: { Duration days: 1. Duration days: 6 }.

]

{ #category : #running }
ScheduleTest >> tearDown [

	DateAndTime localTimeZone: restoredTimeZone.
	super tearDown 

]

{ #category : #tests }
ScheduleTest >> testBetweenAndDoDisjointWithSchedule [
	| count |
	count := 0.
	aSchedule between: (DateAndTime year: 2004 month: 4 day: 1) and: (DateAndTime year: 2004 month: 4 day: 30) do: [ :each | count := count + 1 ].
	self assert: count equals: 0
]

{ #category : #tests }
ScheduleTest >> testBetweenAndDoIncludedInSchedule [
	| count |
	count := 0.
	aSchedule between: (DateAndTime year: 2003 month: 4 day: 1) and: (DateAndTime year: 2003 month: 4 day: 30) do: [ :each | count := count + 1 ].
	self assert: count equals: 8
]

{ #category : #tests }
ScheduleTest >> testBetweenAndDoOverlappingSchedule [
	| count |
	count := 0.
	aSchedule between: (DateAndTime year: 2002 month: 12 day: 1) and: (DateAndTime year: 2003 month: 1 day: 31) do: [ :each | count := count + 1 ].
	self assert: count equals: 8
]

{ #category : #tests }
ScheduleTest >> testDateAndTimes [
	| answer |
	self assert: aSchedule dateAndTimes size equals: 104.
	self assert: aSchedule dateAndTimes first equals: firstEvent.
	answer := true.
	aSchedule dateAndTimes do: [ :each | (each dayOfWeekName = 'Saturday' or: [ each dayOfWeekName = 'Sunday' ]) ifFalse: [ ^ false ] ].
	self assert: answer
]

{ #category : #tests }
ScheduleTest >> testDaysOfWeekAreConsistent [
	| originalDayOfWeekSet modifiedDayOfWeekSet start end |
	originalDayOfWeekSet := (aSchedule dateAndTimes collect: [ :each | each dayOfWeek ]) asSet.
	start := DateAndTime
		year: 2003
		month: 1
		day: 5
		hour: 20
		minute: 30.
	end := DateAndTime
		year: 2003
		month: 6
		day: 4
		hour: 20
		minute: 30.
	modifiedDayOfWeekSet := OrderedCollection new.
	aSchedule between: start and: end do: [ :eachStart | modifiedDayOfWeekSet add: eachStart dayOfWeek ].
	self assert: originalDayOfWeekSet equals: modifiedDayOfWeekSet asSet
]

{ #category : #tests }
ScheduleTest >> testExampleFromSwikiPage [
	"It is often neccessary to schedule repeated events, like airline flight schedules, TV programmes, and file backups.
	 Schedule is a Timespan which maintains an array of Durations.
	 The durations specify the offset to the next scheduled DateAndTime. "
	"Consider a TV programme scheduled for 8:30pm every Saturday and Sunday for the current year. "
	"Find the first Saturday and set its time to 20h30"
	| sat shows |
	sat := Year current asMonth dates detect: [ :d | d dayOfWeekName = #Saturday ].
	sat := sat start + (Duration hours: 20.5).
	"Create a schedule" 
	shows := Schedule starting: sat ending: Year current end.
	shows schedule: { Duration days: 1. Duration days: 6 }.
	"To inspect:"
	shows dateAndTimes.
	shows dateAndTimes collect: [ :dt | dt dayOfWeekName ].


]

{ #category : #tests }
ScheduleTest >> testFromDateAndTime [
	| oc1 oc2 |
	oc1 := OrderedCollection new.
	DateAndTime today to: DateAndTime tomorrow by: 10 hours do: [ :dt | oc1 add: dt ].

	oc2 := {DateAndTime today . (DateAndTime today + 10 hours) . (DateAndTime today + 20 hours)}.

	self assert: oc1 asArray equals: oc2
]

{ #category : #tests }
ScheduleTest >> testIncludes [
	self assert: (aSchedule includes: (DateAndTime year: 2003 month: 6 day: 15 hour: 20 minute: 30 second: 0 offset: 0 hours))
	

]

{ #category : #tests }
ScheduleTest >> testMonotonicity [

	| t1 t2 t3 t4 |
	t1 := DateAndTime now.
	t2 := DateAndTime now.
	t3 := DateAndTime now.
	t4 := DateAndTime now.

	self 
		assert: (	t1 <= t2);
		assert: (	t2 <= t3);
		assert: (	t3 <= t4).

]

{ #category : #tests }
ScheduleTest >> testSchedule [
	self assert: aSchedule schedule size equals: 2.
	self assert: aSchedule schedule first equals: 1 days.
	self assert: aSchedule schedule second equals: 6 days
]
