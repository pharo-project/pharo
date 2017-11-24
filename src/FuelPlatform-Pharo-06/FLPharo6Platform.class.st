Class {
	#name : #FLPharo6Platform,
	#superclass : #FLPharoPlatform,
	#category : #'FuelPlatform-Pharo-06'
}

{ #category : #'private-hacks' }
FLPharo6Platform class >> addHacks [
	"no hacks for development platform"
]

{ #category : #testing }
FLPharo6Platform class >> isResponsibleForCurrentPlatform [
	^ super isResponsibleForCurrentPlatform and: [
		(SystemVersion current type = 'Pharo')
			and: [ SystemVersion current major = 6 ] ]
]

{ #category : #'accessing-files' }
FLPharo6Platform >> deleteFileNamed: aFilename [
	((Smalltalk at: #File) named: aFilename) delete
]

{ #category : #'accessing-files' }
FLPharo6Platform >> fileNamed: aFilename readStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) readStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #'accessing-files' }
FLPharo6Platform >> fileNamed: aFilename writeStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) writeStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #testing }
FLPharo6Platform >> isBigEndian [
	^ (Smalltalk at: #EndianDetector) isBigEndian
]
