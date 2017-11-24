"
I am responsible for storing information about AbsolutePath objects.
"
Class {
	#name : #AbsolutePathStoredSetting,
	#superclass : #StoredSetting,
	#category : #'System-Settings-Persistence-Ston-StoredSettings'
}

{ #category : #accessing }
AbsolutePathStoredSetting >> realValue [
	^ (AbsolutePath withAll: realValue) asFileReference
]
