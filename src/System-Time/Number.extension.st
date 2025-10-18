Extension { #name : 'Number' }

{ #category : '*System-Time' }
Number >> asDuration [

 	^ Duration seconds: self asInteger
]

{ #category : '*System-Time' }
Number >> asSeconds [
	^ Duration milliSeconds: self * 1000
]

{ #category : '*System-Time' }
Number >> day [

 	^ self days
]

{ #category : '*System-Time' }
Number >> days [

 	^ Duration days: self
]

{ #category : '*System-Time' }
Number >> hour [

 	^ self hours
]

{ #category : '*System-Time' }
Number >> hours [

 	^ Duration hours: self
]

{ #category : '*System-Time' }
Number >> milliSecond [
	"1 milliSecond printString >>> '0:00:00:00.001'"
	"(1 second + 1 milliSecond) printString >>> '0:00:00:01.001'"

 	^ self milliSeconds
]

{ #category : '*System-Time' }
Number >> milliSeconds [

	"2 milliSeconds printString >>> '0:00:00:00.002'"
	"(1 second + 2 milliSeconds) printString >>> '0:00:00:01.002'"

 	^ Duration milliSeconds: self
]

{ #category : '*System-Time' }
Number >> minute [
	"1 minute printString >>> '0:00:01:00'"
	"(1 hour + 1 minute) printString >>> '0:01:01:00'"

 	^ self minutes
]

{ #category : '*System-Time' }
Number >> minutes [
	"2 minutes printString >>> '0:00:02:00'"
	"(1 hour + 2 minutes) printString >>> '0:01:02:00'"

 	^ Duration minutes: self
]

{ #category : '*System-Time' }
Number >> nanoSecond [
	"1 nanoSecond printString >>> '0:00:00:00.000000001'"
	"(1 milliSecond + 1 nanoSecond) printString >>> '0:00:00:00.001000001'"

 	^ self nanoSeconds
]

{ #category : '*System-Time' }
Number >> nanoSeconds [
	"2 nanoSeconds printString >>> '0:00:00:00.000000002'"
	"(1 milliSecond + 2 nanoSeconds) printString >>> '0:00:00:00.001000002'"

 	^ Duration nanoSeconds: self
]

{ #category : '*System-Time' }
Number >> second [
	"1 second printString >>> '0:00:00:01'"
	"(1 minute + 1 second) printString >>> '0:00:01:01'"

 	^ self seconds
]

{ #category : '*System-Time' }
Number >> seconds [

	"2 seconds printString >>> '0:00:00:02'"
	"(1 minute + 2 seconds) printString >>> '0:00:01:02'"

 	^ Duration seconds: self
]

{ #category : '*System-Time' }
Number >> week [

 	^ self weeks
]

{ #category : '*System-Time' }
Number >> weeks [

 	^ Duration weeks: self
]

{ #category : '*System-Time' }
Number >> year [

 	^self years
]

{ #category : '*System-Time' }
Number >> years [

 	^ Duration years: self
]
