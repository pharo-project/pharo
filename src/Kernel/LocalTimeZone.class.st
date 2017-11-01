"
I am the local time zone which will use the system's current time offset dynamically.

This is the default timezone.
"
Class {
	#name : #LocalTimeZone,
	#superclass : #AbstractTimeZone,
	#pools : [
		'ChronologyConstants'
	],
	#category : 'Kernel-Chronology'
}

{ #category : #accessing }
LocalTimeZone >> abbreviation [
	^ String streamContents: [ :s |
		s nextPutAll: 'LT'; print: self offset hours; nextPut: $:.
		s nextPutAll: (self offset minutes printPaddedWith: $0 to: 2) ].
]

{ #category : #converting }
LocalTimeZone >> asFixedTimeZone [
	"Convert this dynamic timezone to one with a fixed offset."
	^ TimeZone
		offset: self offset
		name: 'Fixed Local Time'
		abbreviation: self abbreviation
]

{ #category : #accessing }
LocalTimeZone >> name [
	^ 'Local Time'
]

{ #category : #accessing }
LocalTimeZone >> offset [
	^ self primOffset minutes
]

{ #category : #primitives }
LocalTimeZone >> primOffset [
	"The offset from UTC in minutes, with positive offsets being towards the east.
	(San Francisco is in UTC -07*60 and Paris is in UTC +02*60 (daylight savings is not in effect)."
	<primitive:'primitiveTimezoneOffset' module: 'LocalePlugin'>
	^ 0
]
