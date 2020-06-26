"
I describe and implement settings for choosing the emergency debugging tool to use
"
Class {
	#name : #EDEmergencyDebuggerSettings,
	#superclass : #Object,
	#classVars : [
		'defaultEmergencyDebugger'
	],
	#category : #'EmergencyDebugger-Core'
}

{ #category : #accessing }
EDEmergencyDebuggerSettings class >> defaultEmergencyDebugger [
	^defaultEmergencyDebugger ifNil:[defaultEmergencyDebugger := EDEmergencyDebugger]
]

{ #category : #accessing }
EDEmergencyDebuggerSettings class >> defaultEmergencyDebugger: aDebugger [
	defaultEmergencyDebugger := aDebugger value
]

{ #category : #settings }
EDEmergencyDebuggerSettings class >> emergencyDebuggerSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #defaultEmergencyDebugger)
		label: 'Emergency Debugger';
		parent: #debugging;
		description: 'Debugger to use in case of emergency (debugger error)';
		target: self;
		default: EDEmergencyDebugger;
		domainValues:
			{('Ed' -> EDEmergencyDebugger).
			('Emergency Evaluator' -> Transcripter)}
]

{ #category : #'see class side' }
EDEmergencyDebuggerSettings >> seeClassSide [ 
]
