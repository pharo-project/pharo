Class {
	#name : #VSUnloadUnit,
	#superclass : #Object,
	#instVars : [
		'configurationClass',
		'commitVersion'
	],
	#category : #Versionner-Commit
}

{ #category : #'instance creation' }
VSUnloadUnit class >> configurationClass: aClass [
	^ self basicNew 
		initializeConfigurationClass: aClass;
		yourself
]

{ #category : #'instance creation' }
VSUnloadUnit class >> new [
	self error: 'Use #configuration:'
]

{ #category : #accessing }
VSUnloadUnit >> configurationClass [
	^ configurationClass
]

{ #category : #operating }
VSUnloadUnit >> execute [ 
	| developmentVersion project |
	
	developmentVersion := self configurationClass project version: #development. 
	project := MTProject
		newFromVersion: developmentVersion
		inConfiguration: self configurationClass.
		
	(self configurationClass class selectors includes: #preUnload)
		ifTrue: [ self configurationClass perform: #preUnload ].
		
	project packages do: [ :eachPackage | 
		(MCWorkingCopy allManagers 
			detect: [ :each | each packageName = eachPackage name ])
			unload ].
	
	
]

{ #category : #initialization }
VSUnloadUnit >> initializeConfigurationClass: aClass [
	configurationClass := aClass.
	self initialize
]
