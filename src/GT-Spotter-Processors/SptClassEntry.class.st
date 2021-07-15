"
I wrap a Class or Trait entry, to modify its behavior if required for the class
"
Class {
	#name : #SptClassEntry,
	#superclass : #SptEntry,
	#category : #'GT-Spotter-Processors-Entries'
}

{ #category : #testing }
SptClassEntry class >> isDeprecated [
	^ true
]

{ #category : #converting }
SptClassEntry >> asString [

	^ content name
]

{ #category : #evaluating }
SptClassEntry >> doEvaluate [
	content browse
]

{ #category : #accessing }
SptClassEntry >> icon [

	^ content systemIcon
]
