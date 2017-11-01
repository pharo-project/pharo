"
Versionnner commands related to the development workflow.
"
Class {
	#name : #VSDevCycleCommand,
	#superclass : #VSCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #private }
VSDevCycleCommand >> createDevelopmentVersion [
	| newDevVersionString |
	
	newDevVersionString := UIManager default
		request: 'No development version found! (must be a baseline)' , Character cr asString ,
					'Version number for the next development version'
		initialAnswer: 1.0
		title: 'Create a development version'.
	newDevVersionString ifNotNil: [ 
		newDevVersionString := workflow standardizeDevVersionString: newDevVersionString.
		workflow createNextDevelopment: newDevVersionString ]
]

{ #category : #checking }
VSDevCycleCommand >> ensureDevelopmentVersionExists [
	[ workflow developmentVersion ]
		on: MetacelloVersionDoesNotExistError
		do: [ self createDevelopmentVersion ]
]

{ #category : #checking }
VSDevCycleCommand >> ensureDevelopmentVersionIsBaseline [
	| devVersion |
	devVersion := workflow developmentVersion.
	(devVersion blessing == #baseline) 
		ifFalse: [ self createDevelopmentVersion ]
]
