"
Infos about a project
"
Class {
	#name : #MBProjectInfo,
	#superclass : #MBAbstractPackageInfo,
	#instVars : [
		'currentVersion'
	],
	#category : #Versionner-Core-Model
}

{ #category : #accessing }
MBProjectInfo class >> helpLabel [

	^'Project'
]

{ #category : #converting }
MBProjectInfo >> buildStringOrText [
	| string cv projectClass info attributes specVersion |
	string := super buildStringOrText.
	attributes := OrderedCollection new.
	self isDirty
		ifTrue: [ attributes add: TextEmphasis underlined ].
	(((projectClass := self spec projectClass) isNil or: [ (cv := self currentVersion) isEmpty ])
		or: [ (specVersion := self spec versionOrNil) isNil ])
		ifTrue: [ 
			attributes notEmpty
				ifTrue: [ ^ Text string: string attributes: attributes ].
			^ string ].
	self currentVersionMismatch
		ifTrue: [ attributes add: TextEmphasis bold ].
	specVersion versionStatus == #somethingLoaded
		ifTrue: [ attributes add: TextColor red ].
	attributes notEmpty
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string
]

{ #category : #accessing }
MBProjectInfo >> classForBrowsing [

	^self spec projectClass
]

{ #category : #accessing }
MBProjectInfo >> currentVersion [
	currentVersion == nil
		ifTrue: [ 
			| projectClass version info |
			(projectClass := self spec projectClass) == nil
				ifTrue: [ 
					currentVersion := ''.
					^ currentVersion ].
			(info := self configurationRoot configurationInfoFor: projectClass) == nil
				ifTrue: [ 
					currentVersion := ''.
					^ currentVersion ].
			(currentVersion := info currentVersion) == nil
				ifTrue: [ 
					currentVersion := ''.
					^ currentVersion ].
			currentVersion := currentVersion versionString ].
	^ currentVersion
]

{ #category : #accessing }
MBProjectInfo >> currentVersion: anObject [

	currentVersion := anObject
]

{ #category : #testing }
MBProjectInfo >> currentVersionMismatch [
	| cv specVersion |
	(cv := self currentVersion) notEmpty
		ifTrue: [ 
			(specVersion := self spec versionOrNil) ~~ nil
				ifTrue: [^ cv asMetacelloVersionNumber ~= specVersion versionNumber ]].
	^false
]

{ #category : #testing }
MBProjectInfo >> interestedInConfigurationInfo: configInfo [
	| projectClass |
	(projectClass := self spec projectClass) == nil
		ifTrue: [ ^ false ].
	^(self configurationRoot configurationInfoFor: projectClass) name = configInfo name

]

{ #category : #testing }
MBProjectInfo >> interestedInPackageNamed: aString [
	| projectClass version info |
	(projectClass := self spec projectClass) == nil
		ifTrue: [ ^ false ].
	(aString beginsWith: projectClass name asString)
		ifTrue: [ ^ true ].
	(info := self configurationRoot configurationInfoFor: projectClass) == nil
		ifTrue: [ ^ false ].
	^ info interestedInPackageNamed: aString
]

{ #category : #printing }
MBProjectInfo >> printOn: aStream [
	| cv specVersion |
	self isDirty
		ifTrue: [ aStream nextPutAll: '* ' ].
	super printOn: aStream.
	(cv := self currentVersion) isEmpty
		ifTrue: [ aStream nextPutAll: ' ---' ]
		ifFalse: [ 
			(specVersion := self spec versionOrNil) ~~ nil
				ifTrue: [ 
					aStream nextPutAll: ' ' , specVersion versionString.
					cv asMetacelloVersionNumber = specVersion versionNumber
						ifFalse: [ aStream nextPutAll: ' [' , cv , ']' ] ] ]
]

{ #category : #utils }
MBProjectInfo >> repositories [
	"Return the repositories associated with the configuration for the project."

	^self spec projectPackage workingCopy repositoryGroup repositories
]

{ #category : #accessing }
MBProjectInfo >> typeLabel [

	^'project'
]

{ #category : #accessing }
MBProjectInfo >> validate [
	validationResult == nil
		ifTrue: [ 
			validationIcon := nil.
			validationResult := self validateIfAbsent: [ #() ] ].
	^ validationResult
]

{ #category : #accessing }
MBProjectInfo >> validateIfAbsent: absentBlock [

	^MetacelloToolBox validateConfiguration: (Smalltalk 
		at: self packageName asSymbol 
		ifAbsent: absentBlock).

]
