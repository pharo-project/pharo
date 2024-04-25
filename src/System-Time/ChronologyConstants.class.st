"
ChronologyConstants is a SharedPool for the constants used by the Kernel-Chronology classes.
"
Class {
	#name : 'ChronologyConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'DayNames',
		'DaysInMonth',
		'HoursInDay',
		'MicrosecondsInDay',
		'MinutesInHour',
		'MonthNames',
		'NanosInMicroSecond',
		'NanosInMillisecond',
		'NanosInSecond',
		'SecondsInDay',
		'SecondsInHour',
		'SecondsInMinute',
		'Epoch'
	],
	#category : 'System-Time',
	#package : 'System-Time'
}

{ #category : 'class initialization' }
ChronologyConstants class >> initialize [
	"ChronologyConstants initialize"
	Epoch := 2415386. 		"Julian day number of 1 Jan 1901"
	SecondsInDay := 86400.
	MicrosecondsInDay := SecondsInDay * 1e6.
	SecondsInHour := 3600.
	SecondsInMinute := 60.
	MinutesInHour := 60.
	HoursInDay := 24.
	NanosInSecond := 10 raisedTo: 9.
	NanosInMillisecond := 10 raisedTo: 6.
	NanosInMicroSecond := 10 raisedTo: 3.
	DayNames := #(Sunday Monday Tuesday Wednesday Thursday Friday Saturday).

	MonthNames := #(January February March April May June July
 			August September October November December).
	DaysInMonth := #(31 28 31 30 31 30 31 31 30 31 30 31)
]
