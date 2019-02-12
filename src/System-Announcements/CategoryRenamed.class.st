"
This announcement will be emited when renaming a category using:
=> SystemOrganizer >> renameCategory:toBe:
"
Class {
	#name : #CategoryRenamed,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'newCategoryName',
		'oldCategoryName'
	],
	#category : #'System-Announcements-System-Categories'
}

{ #category : #'instance creation' }
CategoryRenamed class >> classCategoryRenamedFrom: anOldCategoryName to: aNewCategoryName [
	^self new
			newCategoryName: aNewCategoryName;
			oldCategoryName: anOldCategoryName;
			yourself
]

{ #category : #accessing }
CategoryRenamed >> newCategoryName [
	^newCategoryName
]

{ #category : #accessing }
CategoryRenamed >> newCategoryName: aCategoryName [
	newCategoryName := aCategoryName
]

{ #category : #accessing }
CategoryRenamed >> oldCategoryName [
	^oldCategoryName
]

{ #category : #accessing }
CategoryRenamed >> oldCategoryName: aCategoryName [
	oldCategoryName := aCategoryName
]
