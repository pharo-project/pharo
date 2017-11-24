"
A command to save all dirty packages of a project, including the configuration.
"
Class {
	#name : #VSSaveProjectCommand,
	#superclass : #VSVersionCommand,
	#classVars : [
		'ValidateBeforeSaving'
	],
	#category : #'Versionner-Core-Commands'
}

{ #category : #settings }
VSSaveProjectCommand class >> enableValidateSetting: aBuilder [
	<systemsettings>
	(aBuilder setting: #validateBeforeSaving)
		label: 'Validate before commit';
		parent: #versionner;
		target: self;
		description: 'When selected the project will be validated (i.e., the metacello rules will be checked)'
]

{ #category : #settings }
VSSaveProjectCommand class >> validateBeforeSaving [

	^ ValidateBeforeSaving ifNil: [ ValidateBeforeSaving := true ]
]

{ #category : #settings }
VSSaveProjectCommand class >> validateBeforeSaving: aBoolean [

	 ValidateBeforeSaving := aBoolean 
]

{ #category : #execute }
VSSaveProjectCommand >> alertValidationErrors: errors for: configClassName [
	| message |
	
	message :=  'There are critical issues in configuration ' , configClassName printString, '. Configuration not saved!' , Character cr asString.
	errors do: [ :error |
		 message := message , Character cr asString, error asString ]. 
	UIManager default 
		alert: message
		title: 'Critical Issues'
]

{ #category : #execute }
VSSaveProjectCommand >> commit [
	Komitter openAndCommitToMonticelloWorkingCopiesFilteredBy: self filterBlock 
]

{ #category : #execute }
VSSaveProjectCommand >> execute [
	| configClassName |
	target
		ifNil: [ self inform: 'Select the version to commit.'.
			^ self ].
	configClassName := self configurationClass name asString.
	self class validateBeforeSaving
		ifTrue: [ (MetacelloToolBox checkForCriticalValidationIssues: configClassName)
				ifEmpty: [ self commit]
				ifNotEmpty: [ :errors | self alertValidationErrors: errors for: configClassName ] ]
		ifFalse: [ self commit ]
]

{ #category : #execute }
VSSaveProjectCommand >> filterBlock [
	| project packageNames |
	
	project := target.
	packageNames := project packages collect: [ :package | package name ].
	packageNames add: project configurationClass name asString.
	^ [ :workingCopy | packageNames includes: workingCopy package name ]

]
