"
I am responsible for storing information about RelativePath objects.
"
Class {
	#name : #RelativePathStoredSetting,
	#superclass : #StoredSetting,
	#category : #'System-Settings-Core-Persistence-Ston-StoredSettings'
}

{ #category : #accessing }
RelativePathStoredSetting >> realValue [
	^ (RelativePath withAll: realValue) asFileReference
]
