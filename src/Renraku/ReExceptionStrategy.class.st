"
The subclasses of this class define how Renraku should handle exceptions signallied by analyzers
"
Class {
	#name : #ReExceptionStrategy,
	#superclass : #Object,
	#classInstVars : [
		'current'
	],
	#category : #'Renraku-Utility'
}

{ #category : #accessing }
ReExceptionStrategy class >> current [

	^ current ifNil: [ current := ReLogExceptionStrategy ]
]

{ #category : #accessing }
ReExceptionStrategy class >> current: aStrategy [

	^ current := aStrategy
]

{ #category : #settings }
ReExceptionStrategy class >> exceptionStrategySettingsOn: aBuilder [
	<systemsettings>
	
	(aBuilder pickOne: #current)
		label: 'Exception handling strategy';
		target: self;
		parent: #qualityAssistant;
		default: ReLogExceptionStrategy;
		description: self settingDescription;
		domainValues: (
			self allSubclasses collect: [ :c |
				c strategyTitle -> c ])
]

{ #category : #'error handling' }
ReExceptionStrategy class >> handle: anException about: anEntity forProppertiesDo: aBlock [
	"do something with anException that occured during anEntity validation.
	in case you want to produce a new renraku property evaluate aBlock with it"
	
	self subclassResponsibility 
]

{ #category : #settings }
ReExceptionStrategy class >> settingDescription [
	
	^ String streamContents: [ :stream |
		stream nextPutAll: 'The selected strategy defines how to handle exceptions that occur during a renraku validation of an entity.'.
		stream cr.
		
		self allSubclassesDo: [ :cls |
			stream
				cr;
				nextPutAll: cls strategyTitle;
				nextPutAll: ': ';
				nextPutAll: cls strategyDescription ] ]
]

{ #category : #accessing }
ReExceptionStrategy class >> strategyDescription [
	"a string that will be used as a part of the setting explenation"

	^ self subclassResponsibility
]

{ #category : #accessing }
ReExceptionStrategy class >> strategyTitle [
	"a string the will be displayed for the setting option"

	^ self subclassResponsibility
]
