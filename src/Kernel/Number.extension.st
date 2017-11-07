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
 
 	^ self milliSeconds
]

{ #category : #'*Kernel-Chronology' }
Number >> milliSeconds [
 
 	^ Duration milliSeconds: self
]

{ #category : #'*Kernel-Chronology' }
Number >> minute [
 
 	^ self minutes
]

{ #category : #'*Kernel-Chronology' }
Number >> minutes [
 
 	^ Duration minutes: self
]

{ #category : #'*Kernel-Chronology' }
Number >> nanoSecond [
 
 	^ self nanoSeconds
]

{ #category : #'*Kernel-Chronology' }
Number >> nanoSeconds [
 
 	^ Duration nanoSeconds: self.
]

{ #category : #'*Kernel-Chronology' }
Number >> second [
 
 	^ self seconds
]

{ #category : #'*Kernel-Chronology' }
Number >> seconds [
 
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
