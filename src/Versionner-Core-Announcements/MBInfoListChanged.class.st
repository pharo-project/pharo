"
The info list changed
"
Class {
	#name : #MBInfoListChanged,
	#superclass : #MBAnnouncement,
	#instVars : [
		'configurationInfo'
	],
	#category : #'Versionner-Core-Announcements'
}

{ #category : #'instance creation' }
MBInfoListChanged class >> changed: aConfigurationInfo [

	^(self new) 
		configurationInfo: aConfigurationInfo;
		yourself
]

{ #category : #accessing }
MBInfoListChanged >> configurationInfo [
	^ configurationInfo
]

{ #category : #accessing }
MBInfoListChanged >> configurationInfo: anObject [
	configurationInfo := anObject
]
