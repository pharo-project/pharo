"
I plug environment with information about critiques:

1) I decorate critiques with various properties 
2) I provide ""critiques"" method group
 
I allow disable method group using class side methods: 

- ClyCriticEnvironmentPlugin disableMethodGroup.
- ClyCriticEnvironmentPlugin enableMethodGroup.

Internal Representation and Key Implementation Points.

    Instance Variables
	providesMethodGroup:		<Boolean>
"
Class {
	#name : #ClyCriticEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#instVars : [
		'providesMethodGroup'
	],
	#classInstVars : [
		'providesMethodGroupByDefault'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #accessing }
ClyCriticEnvironmentPlugin class >> disableMethodGroup [
	providesMethodGroupByDefault := false.
	(ClyNavigationEnvironment currentImage getPlugin: self ifAbsent: [^self])
		providesMethodGroup: false
]

{ #category : #accessing }
ClyCriticEnvironmentPlugin class >> enableMethodGroup [
	providesMethodGroupByDefault := true.
	(ClyNavigationEnvironment currentImage getPlugin: self ifAbsent: [^self])
		providesMethodGroup: true
]

{ #category : #settings }
ClyCriticEnvironmentPlugin class >> isEnabled [
	^self providesMethodGroupByDefault
]

{ #category : #settings }
ClyCriticEnvironmentPlugin class >> isEnabled: aBoolean [
	aBoolean 
		ifTrue: [ self enableMethodGroup ]
		ifFalse: [ self disableMethodGroup ]
]

{ #category : #testing }
ClyCriticEnvironmentPlugin class >> isSlow [
	^true
]

{ #category : #accessing }
ClyCriticEnvironmentPlugin class >> providesMethodGroupByDefault [
	^providesMethodGroupByDefault ifNil: [ false ]
]

{ #category : #settings }
ClyCriticEnvironmentPlugin class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #isEnabled)
		parent: #Calypso;
		default: false;
		label: 'Show critiques method group?';
		description: 'If true, there will be special method group which will analyze all methods of selected classes in the browser. Analysis is performed in the background and should not affect general performance of the system';
		target: self
]

{ #category : #'method groups' }
ClyCriticEnvironmentPlugin >> collectMethodGroupProviders [
	providesMethodGroup ifFalse: [ ^#() ].
	
	^{ClyCriticMethodGroupProvider new}
]

{ #category : #initialization }
ClyCriticEnvironmentPlugin >> initialize [
	super initialize.
	
	providesMethodGroup := self class providesMethodGroupByDefault
]

{ #category : #accessing }
ClyCriticEnvironmentPlugin >> providesMethodGroup [
	^ providesMethodGroup
]

{ #category : #accessing }
ClyCriticEnvironmentPlugin >> providesMethodGroup: anObject [
	providesMethodGroup := anObject
]
