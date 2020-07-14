Class {
	#name : #PRPearOpenedFile,
	#superclass : #PRPearFile,
	#category : #'Pillar-Pear'
}

{ #category : #accessing }
PRPearOpenedFile >> basename [
	^ self file basename
]

{ #category : #accessing }
PRPearOpenedFile >> fullName [
	^ self file fullName
]

{ #category : #testing }
PRPearOpenedFile >> isNew [
	^ false
]

{ #category : #action }
PRPearOpenedFile >> save: aText withExtension: anExtension [
	self file writeStreamDo: [ 
		:str | str nextPutAll: aText text asString; truncate: aText text asString size 
	].
	(self file extension = anExtension asString) ifFalse: [ self file renameTo: self file withoutExtension basename,'.', anExtension asString].
	^ self

]
