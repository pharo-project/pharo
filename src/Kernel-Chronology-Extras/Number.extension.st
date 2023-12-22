Extension { #name : 'Number' }

{ #category : '*Kernel-Chronology-Extras' }
Number >> asDuration [

 	^ Duration seconds: self asInteger
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> asSeconds [
	^ Duration milliSeconds: self * 1000
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> days [

 	^ Duration days: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> hours [

 	^ Duration hours: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> milliSeconds [

	"2 milliSeconds printString >>> '0:00:00:00.002'"
	"(1 second + 2 milliSeconds) printString >>> '0:00:00:01.002'"

 	^ Duration milliSeconds: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> minutes [
	"2 minutes printString >>> '0:00:02:00'"
	"(1 hour + 2 minutes) printString >>> '0:01:02:00'"

 	^ Duration minutes: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> nanoSeconds [
	"2 nanoSeconds printString >>> '0:00:00:00.000000002'"
	"(1 milliSecond + 2 nanoSeconds) printString >>> '0:00:00:00.001000002'"

 	^ Duration nanoSeconds: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> seconds [

	"2 seconds printString >>> '0:00:00:02'"
	"(1 minute + 2 seconds) printString >>> '0:00:01:02'"

 	^ Duration seconds: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> weeks [

 	^ Duration weeks: self
]

{ #category : '*Kernel-Chronology-Extras' }
Number >> years [

 	^ Duration years: self
]
