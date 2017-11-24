"
This announcement will be emited when removing a category using:
=> SystemOrganizer >> removeCategory:
"
Class {
	#name : #CategoryRemoved,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'categoryName'
	],
	#category : #'System-Announcements-System-Categories'
}

{ #category : #'instance creation' }
CategoryRemoved class >> categoryName: aCategoryName [
	^self new
			categoryName: aCategoryName;
			yourself
]

{ #category : #accessing }
CategoryRemoved >> categoryName [
	^categoryName
]

{ #category : #accessing }
CategoryRemoved >> categoryName: aCategoryName [
	categoryName := aCategoryName
]
