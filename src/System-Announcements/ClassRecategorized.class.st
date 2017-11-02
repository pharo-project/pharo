"
This announcement will be raised when:
- we set a category to a class (see 'Class >>category:')
- we set a category to a Trait (see Trait>>category:)

It corresponds to the RecategorizedEvent
"
Class {
	#name : #ClassRecategorized,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'newCategory',
		'oldCategory',
		'classRecategorized'
	],
	#category : 'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassRecategorized class >> class: aClass recategorizedFrom: oldCategory to: newCategory [
	^self new
			classRecategorized: aClass;
			oldCategory: oldCategory;
			newCategory: newCategory;
			yourself
]

{ #category : #accessing }
ClassRecategorized >> classAffected [
	^self classRecategorized
]

{ #category : #accessing }
ClassRecategorized >> classRecategorized [
	^classRecategorized
]

{ #category : #accessing }
ClassRecategorized >> classRecategorized: aClass [
	classRecategorized := aClass
]

{ #category : #accessing }
ClassRecategorized >> newCategory [ 
	^newCategory
]

{ #category : #accessing }
ClassRecategorized >> newCategory: aNewCategory [
	newCategory := aNewCategory
]

{ #category : #accessing }
ClassRecategorized >> oldCategory [
	^oldCategory
]

{ #category : #accessing }
ClassRecategorized >> oldCategory: anOldCategory [
	oldCategory := anOldCategory
]
