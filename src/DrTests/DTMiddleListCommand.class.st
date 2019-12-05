"
I am an abstract command concerning DrTests' middle list.
"
Class {
	#name : #DTMiddleListCommand,
	#superclass : #DTCommand,
	#category : #'DrTests-Commands'
}

{ #category : #accessing }
DTMiddleListCommand >> selectedItems [
	^ self drTest selectedItems
]
