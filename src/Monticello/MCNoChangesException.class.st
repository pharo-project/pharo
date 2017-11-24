"
Notify that no changes have to be merged
"
Class {
	#name : #MCNoChangesException,
	#superclass : #Exception,
	#category : #'Monticello-Versioning'
}

{ #category : #accessing }
MCNoChangesException >> defaultAction [
	self inform: 'No changes'
]
