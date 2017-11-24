"
This announcement will be emited when adding a category using:
=> SystemOrganizer >> addCategory:
"
Class {
	#name : #CategoryAdded,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'categoryName'
	],
	#category : #'System-Announcements-System-Categories'
}

{ #category : #'instance creation' }
CategoryAdded class >> categoryName: aCategoryName [
	^self new
			categoryName: aCategoryName;
			yourself
]

{ #category : #accessing }
CategoryAdded >> categoryName [

	^ categoryName
]

{ #category : #accessing }
CategoryAdded >> categoryName: aCategoryName [
	categoryName := aCategoryName
]
