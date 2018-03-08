"
I serialize or materialize objects using FUEL framework. Create instance of me calling GlobalIdentifierPersistence fuel.
"
Class {
	#name : #GlobalIdentifierFuelPersistence,
	#superclass : #GlobalIdentifierPersistence,
	#category : #'System-Identification'
}

{ #category : #configuration }
GlobalIdentifierFuelPersistence >> fileName [ 
	^ 'org.pharo.gt.spotter.event.recorder.fuel'
]

{ #category : #'load and saving' }
GlobalIdentifierFuelPersistence >> load [
	^ FLMaterializer materializeFromFileNamed: self preferences fullName
]

{ #category : #'load and saving' }
GlobalIdentifierFuelPersistence >> save: aDictionary into: aWriteStream [
	[ FLSerializer serialize: aDictionary on: aWriteStream ]
		on: Error
		do: [ :anError | "ignore" ]
]
