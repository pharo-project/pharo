"
Abstract info for a version in Versionner
"
Class {
	#name : #MBAbstractVersionInfo,
	#superclass : #MBAbstractInfoList,
	#instVars : [
		'configurationInfo',
		'version'
	],
	#category : #Versionner-Core-Model
}

{ #category : #'instance creation' }
MBAbstractVersionInfo class >> new [
	self error: 'Please use version:browser: or version: instead'
]

{ #category : #'instance creation' }
MBAbstractVersionInfo class >> version: aMetacelloVersion [

	^ super new 
		version: aMetacelloVersion;
		yourself
]

{ #category : #private }
MBAbstractVersionInfo >> calculateInfoList [
	| answer list cvs cv |
	answer := OrderedCollection new.
	cvs := ''.
	(cv := self versionForInfoList) 
		ifNil:  [cvs := cv versionString, ' '].
	(list := self projects) 
		ifNotEmpty: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'projects --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self packages)
		ifNotEmpty: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'packages --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self groups)
	 		ifNotEmpty: [
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'groups --').
			answer addAll: list ].
	^ answer
]

{ #category : #commands }
MBAbstractVersionInfo >> cmdBrowse [
	<command>
	^ VSBrowseVersionCommand
]

{ #category : #commands }
MBAbstractVersionInfo >> cmdLoad [
	<command>
	^ VSLoadCommand
]

{ #category : #commands }
MBAbstractVersionInfo >> cmdMakeDevelopment [
	<command>
	^ VSMakeDevelopmentCommand
]

{ #category : #commands }
MBAbstractVersionInfo >> cmdMakeStable [
	<command>
	^ VSMakeStableCommand
]

{ #category : #commands }
MBAbstractVersionInfo >> cmdRemove [
	<command>
	^ VSRemoveCommand
]

{ #category : #accessing }
MBAbstractVersionInfo >> configurationBasename [
	^ self configurationClass name configurationBaseName
]

{ #category : #accessing }
MBAbstractVersionInfo >> configurationClass [
	^ self configurationInfo configurationClass
]

{ #category : #accessing }
MBAbstractVersionInfo >> configurationInfo [
	^ configurationInfo
]

{ #category : #accessing }
MBAbstractVersionInfo >> configurationInfo: anObject [
	configurationInfo := anObject
]

{ #category : #accessing }
MBAbstractVersionInfo >> currentVersion [

	^self configurationInfo currentVersion
]

{ #category : #testing }
MBAbstractVersionInfo >> hasValidationIssues [
	validationResult == nil ifTrue: [ ^false ].
	^validationResult notEmpty
]

{ #category : #testing }
MBAbstractVersionInfo >> isBaseline [
	^ self version blessing == #baseline
]

{ #category : #testing }
MBAbstractVersionInfo >> isBleedingEdge [
	^  (self project hasVersion: #bleedingEdge) and: [
		(self project version: #bleedingEdge) = self version ]
]

{ #category : #testing }
MBAbstractVersionInfo >> isCurrentVersion [
	|cv|
	(cv := self currentVersion) ~~ nil
		ifTrue: [ 
			cv = self version
				ifTrue: [^ true]].
	^false.
]

{ #category : #testing }
MBAbstractVersionInfo >> isDevelopment [
	^  (self project hasVersion: #development) and: [
		(self project version: #development) = self version ]
]

{ #category : #testing }
MBAbstractVersionInfo >> isStable [
	^ (self project hasVersion: #stable) and: [ 
		(self project version: #stable) = self version ]
]

{ #category : #printing }
MBAbstractVersionInfo >> printOn: aStream [
	| cv |
	(cv := self currentVersion) ~~ nil
		ifTrue: [ 
			cv = self version
				ifTrue: [ aStream nextPutAll: ' @ ' ] ].
	super printOn: aStream.

]

{ #category : #accessing }
MBAbstractVersionInfo >> project [
	"Answer the MetacelloProject associated with the receiver."
	
	^self version project
]

{ #category : #accessing }
MBAbstractVersionInfo >> validate [
	validationResult == nil
		ifTrue: [ 
			validationIcon := nil.
			validationResult := MetacelloToolBox validateProject: self project version: self version versionString ].
	^ validationResult
]

{ #category : #accessing }
MBAbstractVersionInfo >> version [
	^ version
]

{ #category : #accessing }
MBAbstractVersionInfo >> version: aMetacelloVersion [

	version := aMetacelloVersion.
	self name: aMetacelloVersion versionString.
	

]

{ #category : #accessing-computed }
MBAbstractVersionInfo >> versionForInfoList [

	^self version 
]

{ #category : #accessing }
MBAbstractVersionInfo >> versionString [
	^ self version versionString
]
