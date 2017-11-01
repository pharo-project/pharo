"
I serialize or materialize objects using STON framework. Create instance of me calling GlobalIdentifierPersistence ston.
"
Class {
	#name : #GlobalIdentifierStonPersistence,
	#superclass : #GlobalIdentifierPersistence,
	#category : 'System-Identification'
}

{ #category : #configuration }
GlobalIdentifierStonPersistence >> fileName [
	^ 'org.pharo.global-identifiers.ston'
]

{ #category : #'load and saving' }
GlobalIdentifierStonPersistence >> load [
	^ self preferences readStreamDo: [ :aReadStream |
			STON fromStream: aReadStream ]
]

{ #category : #'load and saving' }
GlobalIdentifierStonPersistence >> save: aDictionary into: aWriteStream [
	STON put: aDictionary onStreamPretty: aWriteStream
]
