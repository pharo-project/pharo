Extension { #name : #Number }

{ #category : #'*Kernel-Chronology' }
Number >> asDuration [
 
 	^ Duration seconds: self asInteger
 
]

{ #category : #'*Kernel-Chronology' }
Number >> asSeconds [
	^ Duration milliSeconds: self * 1000
]

{ #category : #'*Kernel-Chronology' }
Number >> day [
 
 	^ self days
]

{ #category : #'*Kernel-Chronology' }
Number >> days [
 
 	^ Duration days: self
]

{ #category : #'*Kernel-Chronology' }
Number >> hour [
 
 	^ self hours
]

{ #category : #'*Kernel-Chronology' }
Number >> hours [
 
 	^ Duration hours: self
]

{ #category : #'*Kernel-Chronology' }
Number >> milliSecond [
	"1 milliSecond printString >>> '0:00:00:00.001'"
	"(1 second + 1 milliSecond) printString >>> '0:00:00:01.001'"
	
 	^ self milliSeconds
 
]

{ #category : #'*Kernel-Chronology' }
Number >> milliSeconds [

	"2 milliSeconds printString >>> '0:00:00:00.002'"
	"(1 second + 2 milliSeconds) printString >>> '0:00:00:01.002'"
	
 	^ Duration milliSeconds: self
 
]

{ #category : #'*Kernel-Chronology' }
Number >> minute [
	"1 minute printString >>> '0:00:01:00'"
	"(1 hour + 1 minute) printString >>> '0:01:01:00'"
 
 	^ self minutes
 
]

{ #category : #'*Kernel-Chronology' }
Number >> minutes [
	"2 minutes printString >>> '0:00:02:00'"
	"(1 hour + 2 minutes) printString >>> '0:01:02:00'"
 
 	^ Duration minutes: self
]

{ #category : #'*Kernel-Chronology' }
Number >> nanoSecond [
	"1 nanoSecond printString >>> '0:00:00:00.000000001'"
	"(1 milliSecond + 1 nanoSecond) printString >>> '0:00:00:00.001000001'"
 
 	^ self nanoSeconds
 
]

{ #category : #'*Kernel-Chronology' }
Number >> nanoSeconds [
	"2 nanoSeconds printString >>> '0:00:00:00.000000002'"
	"(1 milliSecond + 2 nanoSeconds) printString >>> '0:00:00:00.001000002'"
 
 	^ Duration nanoSeconds: self.
]

{ #category : #'*Kernel-Chronology' }
Number >> second [
	"1 second printString >>> '0:00:00:01'"
	"(1 minute + 1 second) printString >>> '0:00:01:01'"
	
 	^ self seconds
 
]

{ #category : #'*Kernel-Chronology' }
Number >> seconds [

	"2 seconds printString >>> '0:00:00:02'"
	"(1 minute + 2 seconds) printString >>> '0:00:01:02'"
 
 	^ Duration seconds: self
]

{ #category : #'*Kernel-Chronology' }
Number >> week [
 
 	^ self weeks
 
]

{ #category : #'*Kernel-Chronology' }
Number >> weeks [
 
 	^ Duration weeks: self
]

{ #category : #'*Kernel-Chronology' }
Number >> year [
 
 	^self years
]

{ #category : #'*Kernel-Chronology' }
Number >> years [
 
 	^ Duration years: self
]
