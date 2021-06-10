"
I am an abstract command concerning DrTests' middle list.
"
Class {
	#name : #DTMiddleListCommand,
	#superclass : #DTCommand,
	#category : #'DrTests-Commands'
}

{ #category : #testing }
DTMiddleListCommand class >> isAbstract [ 

	^ self name = #DTMiddleListCommand
]

{ #category : #accessing }
DTMiddleListCommand >> selectedItems [

	^ self context selectedItems
]
