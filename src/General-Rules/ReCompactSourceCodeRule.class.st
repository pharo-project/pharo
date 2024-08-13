"
A lint rule to check for unnecessary separators at end of source code lines and unnecessary dot at end of method
"
Class {
	#name : 'ReCompactSourceCodeRule',
	#superclass : 'ReAbstractRule',
	#classVars : [
		'CriticizeFinalDotInMethodBody'
	],
	#category : 'General-Rules-Clean Code',
	#package : 'General-Rules',
	#tag : 'Clean Code'
}

{ #category : 'testing - interest' }
ReCompactSourceCodeRule class >> checksMethod [

	^ true
]

{ #category : 'settings' }
ReCompactSourceCodeRule class >> compactSourceCodeSettingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #criticizeFinalDotInMethodBody)
		order: 15;
		label: 'Criticize final dot in method body';
		description: 'Define if the system laments about an unnecessary final dot at the end of method body or not when checking for unnecessary separators';
		parent: #qualityAssistant;
		default: true;
		target: self
]

{ #category : 'settings - accessing' }
ReCompactSourceCodeRule class >> criticizeFinalDotInMethodBody [

	^ CriticizeFinalDotInMethodBody ifNil: [ CriticizeFinalDotInMethodBody := true ]
]

{ #category : 'settings - accessing' }
ReCompactSourceCodeRule class >> criticizeFinalDotInMethodBody: aBoolean [
	"If true (default) the rule will lament about unnecessary dots at the end of method body.
	 If false they will be ignored and kept"

	CriticizeFinalDotInMethodBody := aBoolean
]

{ #category : 'accessing' }
ReCompactSourceCodeRule class >> group [

	^ 'Clean Code'
]

{ #category : 'class initialization' }
ReCompactSourceCodeRule class >> initialize [

	CriticizeFinalDotInMethodBody := true.
	self enabled: false
]

{ #category : 'accessing' }
ReCompactSourceCodeRule class >> ruleName [
	^ 'Unnecessary characters found in method body'
]

{ #category : 'accessing' }
ReCompactSourceCodeRule class >> severity [

	^ #information
]

{ #category : 'running' }
ReCompactSourceCodeRule >> basicCheck: aMethod [

	^ ReMethodSourceCleaner isNecessaryToClean: aMethod allowFinalDot: self class criticizeFinalDotInMethodBody not
]

{ #category : 'running' }
ReCompactSourceCodeRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(self basicCheck: aMethod)
		ifTrue: [
			aCriticBlock cull: (
				ReCompactSourceCodeCritic
					for: aMethod
					by: self)  ]
]

{ #category : 'testing' }
ReCompactSourceCodeRule >> providesChange [

	^ true
]
