"
TimeZone is a simple class to colect the information identifying a UTC time zone.

offset			-	Duration	- the time zone's offset from UTC
abbreviation	-	String		- the abbreviated name for the time zone.
name			-	String		- the name of the time zone.

TimeZone class >> #timeZones returns an array of the known time zones
TimeZone class >> #default returns the default time zone (Grenwich Mean Time)
"
Class {
	#name : #TimeZone,
	#superclass : #AbstractTimeZone,
	#instVars : [
		'offset',
		'abbreviation',
		'name'
	],
	#pools : [
		'ChronologyConstants'
	],
	#category : #'Kernel-Chronology'
}

{ #category : #accessing }
TimeZone class >> abbreviated: aString [
 	"Return the timezone whose abbreviation is aString."
 	^ self timeZones detect: [ :timeZone | 
		timeZone abbreviation = aString ].
]

{ #category : #accessing }
TimeZone class >> default [
 	"Answer the default time zone - GMT"
 
 	^ self timeZones detect: [ :timeZone |
		timeZone offset = Duration zero ]
 
]

{ #category : #accessing }
TimeZone class >> local [
	^ LocalTimeZone new
]

{ #category : #accessing }
TimeZone class >> offset: aDuration [
 	"Return the timezone with the given offset
	
		TimeZone offset: 2 hours
	"
 	^ self timeZones 
		detect: [ :timeZone | timeZone offset = aDuration ]
		ifNone: [ self new offset: aDuration ]
]

{ #category : #'instance creation' }
TimeZone class >> offset: aDuration name: aName abbreviation: anAbbreviation [
 
 	^ self new
 		offset: aDuration;
 		name: aName;
 		abbreviation: anAbbreviation;
 		yourself
]

{ #category : #accessing }
TimeZone class >> timeZones [

	^ {
		self offset:  0 hours name: 'Universal Time'              abbreviation: 'UTC'.
		self offset:  0 hours name: 'Greenwich Mean Time'         abbreviation: 'GMT'.
		self offset:  1 hours name: 'British Summer Time'         abbreviation: 'BST'.
		self offset:  2 hours name: 'South African Standard Time' abbreviation: 'SAST'.
		self offset: -8 hours name: 'Pacific Standard Time'       abbreviation: 'PST'.
		self offset: -7 hours name: 'Pacific Daylight Time'       abbreviation: 'PDT'.
		self offset: -7 hours name: 'Mountain Standard Time'      abbreviation: 'MST'.
		self offset: -6 hours name: 'Mountain Daylight Time'      abbreviation: 'MDT'.
		self offset: -6 hours name: 'Central Standard Time'       abbreviation: 'CST'.
		self offset: -5 hours name: 'Central Daylight Time'       abbreviation: 'CDT'.
		self offset: -5 hours name: 'Eastern Standard Time'       abbreviation: 'EST'.
		self offset: -4 hours name: 'Eastern Daylight Time'       abbreviation: 'EDT'.
	}


]

{ #category : #accessing }
TimeZone >> abbreviation [
 
 	^ abbreviation
 
]

{ #category : #accessing }
TimeZone >> abbreviation: aString [

	abbreviation := aString

]

{ #category : #accessing }
TimeZone >> name [
 
 	^ name
 
]

{ #category : #accessing }
TimeZone >> name: aString [

	name := aString

]

{ #category : #accessing }
TimeZone >> offset [
 
 	^ offset
]

{ #category : #accessing }
TimeZone >> offset: aDuration [

	offset := aDuration
]
