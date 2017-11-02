Class {
	#name : #MetacelloScriptGitBasedDownloadNotification,
	#superclass : #Notification,
	#instVars : [
		'projectPath',
		'versionString'
	],
	#category : 'Metacello-Core-Scripts'
}

{ #category : #accessing }
MetacelloScriptGitBasedDownloadNotification >> projectPath [
	^ projectPath
]

{ #category : #accessing }
MetacelloScriptGitBasedDownloadNotification >> projectPath: anObject [
	projectPath := anObject
]

{ #category : #accessing }
MetacelloScriptGitBasedDownloadNotification >> versionString [
	^ versionString
]

{ #category : #accessing }
MetacelloScriptGitBasedDownloadNotification >> versionString: anObject [
	versionString := anObject
]
