"
Set the description on the selected development version
"
Class {
	#name : #VSSetDescriptionCommand,
	#superclass : #VSVersionCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
VSSetDescriptionCommand >> execute [
	|  message |
	message := UIManager default 
					multilineRequest: 'Enter description' initialAnswer: ''.
	(message isNil or: [ message isEmpty ])
		ifTrue: [ ^ self ].
	self executeWithMessage: message
]

{ #category : #execute }
VSSetDescriptionCommand >> executeWithMessage: message [
	| toolbox |
	toolbox := MetacelloToolBox configurationNamed: target configurationBasename.
	toolbox
		updateVersionMethodForVersion: target version versionString
		updateProjects: false
		updatePackages: false
		versionSpecsDo: [ :attribute :versionSpec | 
			attribute == #common
				ifTrue: [ 
					"update the author, description and timeStamp"
					versionSpec
						author: MetacelloPlatform current authorName;
						timestamp: MetacelloPlatform current timestamp.
					MetacelloToolBox appendDescription: message to: versionSpec ].
			true ].
	toolbox commitMethod

]

{ #category : #accessing }
VSSetDescriptionCommand >> title [
	^ 'set description'
]
