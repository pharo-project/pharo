Class {
	#name : #FLPharo7Platform,
	#superclass : #FLPharoPlatform,
	#category : #'FuelPlatform-Pharo-07'
}

{ #category : #'private-hacks' }
FLPharo7Platform class >> addHacks [
	"no hacks for development platform"
]

{ #category : #testing }
FLPharo7Platform class >> isResponsibleForCurrentPlatform [
	^ super isResponsibleForCurrentPlatform and: [
		(SystemVersion current type = 'Pharo')
			and: [ SystemVersion current major = 7 ] ]
]

{ #category : #'accessing-files' }
FLPharo7Platform >> deleteFileNamed: aFilename [
	((Smalltalk at: #File) named: aFilename) delete
]

{ #category : #'accessing-files' }
FLPharo7Platform >> fileNamed: aFilename readStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) readStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #'accessing-files' }
FLPharo7Platform >> fileNamed: aFilename writeStreamDo: aBlock [
	^ ((Smalltalk at: #File) named: aFilename) writeStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]
]

{ #category : #testing }
FLPharo7Platform >> isBigEndian [
	^ (Smalltalk at: #EndianDetector) isBigEndian
]
