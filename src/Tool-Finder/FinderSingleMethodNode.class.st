"
A FinderSingleMethodNode is a node used to display a selector implemented only once in the system
"
Class {
	#name : #FinderSingleMethodNode,
	#superclass : #FinderMethodNode,
	#category : #'Tool-Finder'
}

{ #category : #accessing }
FinderSingleMethodNode >> childrenItems [

	^ #()
]

{ #category : #printing }
FinderSingleMethodNode >> displayString [
	^ self itemMethod selector , ' (' , self itemMethod methodClass name , ')'
]

{ #category : #private }
FinderSingleMethodNode >> isSingle [

	^ true
]

{ #category : #private }
FinderSingleMethodNode >> itemMethod [
	| itemClass |
	itemClass := (self model resultDictionary at: self item) first.
	^ itemClass >> self item
]
