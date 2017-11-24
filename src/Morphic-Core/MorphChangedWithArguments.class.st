"
I am an announcement raised when a Morph changed and need to propagate the information to dependents. In addition, I also propagate a value
"
Class {
	#name : #MorphChangedWithArguments,
	#superclass : #MorphChanged,
	#instVars : [
		'arguments'
	],
	#category : #'Morphic-Core-Announcements'
}

{ #category : #accessing }
MorphChangedWithArguments >> arguments [
	^ arguments
]

{ #category : #accessing }
MorphChangedWithArguments >> arguments: anObject [
	arguments := anObject
]

{ #category : #delivering }
MorphChangedWithArguments >> deliverTo: aHandler [

	^ aHandler update: selector with: arguments

	
]
