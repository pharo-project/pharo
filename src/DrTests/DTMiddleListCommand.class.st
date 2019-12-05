Class {
	#name : #DTMiddleListCommand,
	#superclass : #DTCommand,
	#category : #'DrTests-Commands'
}

{ #category : #accessing }
DTMiddleListCommand >> selectedItems [
	^ self drTest selectedItems
]
