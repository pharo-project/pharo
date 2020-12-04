"
TKTConfiguration is a dynamic variable thread local and thread safe. 

This configuration works as a global configuration for the TaskIt library.

The current value is always pointing a TKTProfile entity. 

For changing the actual profile, go to the settings browser, Taskit group

For defining profiles, add a class method in TKTProfile


"
Class {
	#name : #TKTConfiguration,
	#superclass : #DynamicVariable,
	#instVars : [
		'value'
	],
	#category : #'TaskIt-Profiles'
}

{ #category : #accessing }
TKTConfiguration class >> errorHandler [
	^ self optionAt: #errorHandler
]

{ #category : #accessing }
TKTConfiguration class >> errorHandler: anErrorHandler during: aBlock [
	^ self optionAt: #errorHandler value: anErrorHandler during: aBlock
]

{ #category : #accessing }
TKTConfiguration class >> optionAt: aKey [
	^ self value value at: aKey
]

{ #category : #accessing }
TKTConfiguration class >> optionAt: aKey value: aValue during: aBlock [
	| copy |
	copy := self value copy.
	copy value at: aKey put: aValue.
	^ self value: copy during: aBlock
]

{ #category : #accessing }
TKTConfiguration class >> poolWorkerProcess [
	^ self optionAt: #poolWorkerProcess
]

{ #category : #accessing }
TKTConfiguration class >> poolWorkerProcess: anObject during: aBlock [
	^ self optionAt: #poolWorkerProcess value: anObject during: aBlock
]

{ #category : #accessing }
TKTConfiguration class >> process [
	^ self optionAt: #process
]

{ #category : #accessing }
TKTConfiguration class >> process: anObject during: aBlock [
	^ self optionAt: #process value: anObject during: aBlock
]

{ #category : #accessing }
TKTConfiguration class >> processProvider [
	^ self optionAt: #processProvider
]

{ #category : #accessing }
TKTConfiguration class >> processProvider: aProcessProvider during: aBlock [

	^ self 
		optionAt: #processProvider 
		value: aProcessProvider 
		during: aBlock
]

{ #category : #accessing }
TKTConfiguration class >> profile [
	^ #Hola
]

{ #category : #accessing }
TKTConfiguration class >> profile: aProfile [
	self soleInstance basicValue ifNotNil: [ :p | p finalizeProfile ].
	self soleInstance value: aProfile.
	aProfile activateProfile
]

{ #category : #accessing }
TKTConfiguration class >> profile: aProfile during: aBlock [
	| return |
	
	return := self value: aProfile during: [aProfile activateProfile. aBlock value].
	aProfile finalizeProfile.
	^ return
]

{ #category : #accessing }
TKTConfiguration class >> profileNamed [
	^ self value value name
]

{ #category : #accessing }
TKTConfiguration class >> profileNamed: aProfileName [
	| profile |

	profile := (self profiles detect: [ :ass | ass key = aProfileName ]) value value.
	profile name: aProfileName.
	self profile: profile
]

{ #category : #accessing }
TKTConfiguration class >> profileNamed: aProfileName during: aBlock [
	| return profile |
	profile := (self profiles detect: [ :ass | ass key = aProfileName ]) value value .
	profile name: aProfileName.
	return := self
		value: profile
		during: [ profile activateProfile.
			aBlock value ].
	profile finalizeProfile.
	^ return
]

{ #category : #accessing }
TKTConfiguration class >> profiles [
	^ TKTProfile profiles
]

{ #category : #accessing }
TKTConfiguration class >> resetSoleInstance [ 

	TKTWatchDog reset.
	super resetSoleInstance.
]

{ #category : #accessing }
TKTConfiguration class >> runner [
	^ self optionAt: #runner
]

{ #category : #accessing }
TKTConfiguration class >> runner: aRunner during: aBlock [
	^ self optionAt: #runner value: aRunner during: aBlock
]

{ #category : #accessing }
TKTConfiguration class >> serviceManager [
	^ self optionAt: #serviceManager
]

{ #category : #accessing }
TKTConfiguration class >> serviceManager: aManager during: aBlock [
	^ self optionAt: #serviceManager value: aManager during: aBlock
]

{ #category : #settings }
TKTConfiguration class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder group: #taskitExecution)
		label: 'Task it execution profile';
		with: [ 
			(aBuilder pickOne: #profileNamed)
				label: 'Exeucting Profile';
				target: self;
				parent: #taskitExecution;
				default: TKTProfile defaultProfileName;
				order: 1;
				domainValues: (self profiles collect: #key) ]
]

{ #category : #accessing }
TKTConfiguration class >> watchDog [
	^ self optionAt: #watchDog
]

{ #category : #accessing }
TKTConfiguration >> basicValue [ 
	^ value
]

{ #category : #accessing }
TKTConfiguration >> default [
	
	^ value ifNil: [ 
			self class profile: TKTProfile defaultProfile.
			value ifNil: [ 
				self error: 'No default profile available.' ] ]
]

{ #category : #accessing }
TKTConfiguration >> value: aValue [
	value := aValue
]
