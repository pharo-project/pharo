"
A configuration info changed
"
Class {
	#name : #MBConfigurationInfoChanged,
	#superclass : #MBAnnouncement,
	#instVars : [
		'configurationInfo'
	],
	#category : #'Versionner-Core-Announcements'
}

{ #category : #'instance creation' }
MBConfigurationInfoChanged class >> changed: aConfigurationInfo [

	^(self new) 
		configurationInfo: aConfigurationInfo;
		yourself
]

{ #category : #accessing }
MBConfigurationInfoChanged >> configurationInfo [
	^ configurationInfo
]

{ #category : #accessing }
MBConfigurationInfoChanged >> configurationInfo: anObject [
	configurationInfo := anObject 
	
]
