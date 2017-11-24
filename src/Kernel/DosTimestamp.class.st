"
DOS stores timestamps, in local time, as 32 bit integers with the following format:
	32 bits (low to high):
		Low 16 bits:
			Bits 0-4: seconds / 2
	 		Bits 5-10: minutes 0-59
			Bits 11-15: hours 0-23
		High 16 bits:
			16-20: day of month 1-31
	 		21-24: month 1-12
			25-31: year offset from 1980 (e.g. 1981 -> 1)

References (with visual aids): http://blogs.msdn.com/b/oldnewthing/archive/2003/09/05/54806.aspx & http://mindprod.com/jgloss/zip.html
"
Class {
	#name : #DosTimestamp,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'Kernel-Chronology'
}

{ #category : #private }
DosTimestamp class >> dateValueFrom: aDate [

	| dosYear dosMonth dosDay |
	dosYear := aDate year - DateAndTime dosEpoch year << 9.
	dosMonth := aDate monthIndex << 5.
	dosDay := aDate dayOfMonth.
	^ dosYear + dosMonth + dosDay.
]

{ #category : #accessing }
DosTimestamp class >> epoch [

	^ DateAndTime dosEpoch
]

{ #category : #'instance creation' }
DosTimestamp class >> fromDateAndTime: aDateAndTime [ 

	| dateValue timeValue local |
	local := aDateAndTime asLocal.
	dateValue := self dateValueFrom: local asDate.
	timeValue := self timeValueFrom: local asTime.
	^ self on: (dateValue << 16) + timeValue.
]

{ #category : #'instance creation' }
DosTimestamp class >> on: anInteger [

	^ self new initializeValue: anInteger.
]

{ #category : #private }
DosTimestamp class >> timeValueFrom: aTime [

	| dosHours dosMinutes dosSeconds |
	dosHours := aTime hour << 11.
	dosMinutes := aTime minute << 5.
	dosSeconds := (aTime second / 2) floor.
	^ dosHours + dosMinutes + dosSeconds.
]

{ #category : #comparing }
DosTimestamp >> = rhs [

	^ self value = rhs value.
]

{ #category : #converting }
DosTimestamp >> asDateAndTime [

	^ DateAndTime date: self date time: self time.
]

{ #category : #private }
DosTimestamp >> date [
	"See class comment for format details"

	| encodedDate yearsSinceDosEpoch month day year |
	encodedDate := self high16Bits.
	yearsSinceDosEpoch := encodedDate >> 9. "High 7 bits"
	month := encodedDate >> 5 & 2r1111. "Middle 4 bits"
	day := encodedDate & 2r11111. "Low 5 bits"
	year := self epoch year + yearsSinceDosEpoch.
	^ Date year: year month: month day: day. 
]

{ #category : #private }
DosTimestamp >> epoch [

	^ self class epoch.
]

{ #category : #comparing }
DosTimestamp >> hash [

	^ self value hash.
]

{ #category : #private }
DosTimestamp >> high16Bits [

	^ value >> 16.
]

{ #category : #private }
DosTimestamp >> initializeValue: anInteger [ 

	value := anInteger.
	^ self.
]

{ #category : #private }
DosTimestamp >> low16Bits [
	
	^ value & 2r1111111111111111.
]

{ #category : #printing }
DosTimestamp >> printOn: aStream [

	aStream
		nextPutAll: self asDateAndTime printString;
		nextPutAll: ' (';
		nextPutAll: self value asString;
		nextPut: $).
]

{ #category : #private }
DosTimestamp >> time [
	"See class comment for format details"
	
	| encodedTime hours minutes secondsHalved seconds |
	encodedTime := self low16Bits.
	hours := encodedTime >> 11. "High 5 bits"
	minutes := encodedTime >> 5 & 2r111111. "Middle 6 bits"
	secondsHalved := encodedTime & 2r11111. "Low 5 bits"
	seconds := (secondsHalved * 2) floor.
	^ Time hour: hours minute: minutes second: seconds.
]

{ #category : #accessing }
DosTimestamp >> value [

	^ value.
]
