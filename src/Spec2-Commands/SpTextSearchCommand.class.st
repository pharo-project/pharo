Class {
	#name : #SpTextSearchCommand,
	#superclass : #SpToolCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #accessing }
SpTextSearchCommand class >> allOrdered [

	^ self allSubclasses sorted: #order ascending
]

{ #category : #default }
SpTextSearchCommand class >> defaultName [

	^ 'Find'
]

{ #category : #accessing }
SpTextSearchCommand class >> order [
	"at the end by default"

	^ 999
]
