Class {
	#name : #SpDirectionableLayout,
	#superclass : #SpExecutableLayout,
	#instVars : [
		'direction'
	],
	#category : #'Spec2-Layout-Paned'
}

{ #category : #'instance creation' }
SpDirectionableLayout class >> new [ 

	^ super new beVertical
]

{ #category : #'instance creation' }
SpDirectionableLayout class >> newHorizontal [ 

	^ self newLeftToRight 
]

{ #category : #'instance creation' }
SpDirectionableLayout class >> newLeftToRight [

	^ super new beHorizontal
]

{ #category : #'instance creation' }
SpDirectionableLayout class >> newTopToBottom [

	^ super new beVertical
]

{ #category : #'instance creation' }
SpDirectionableLayout class >> newVertical [ 

	^ self newTopToBottom 
]

{ #category : #initialization }
SpDirectionableLayout >> beHorizontal [

	direction := SpLayoutDirection horizontal
]

{ #category : #initialization }
SpDirectionableLayout >> beLeftToRight [

	direction := SpLayoutDirection horizontal
]

{ #category : #initialization }
SpDirectionableLayout >> beTopToBottom [

	direction := SpLayoutDirection vertical
]

{ #category : #initialization }
SpDirectionableLayout >> beVertical [

	direction := SpLayoutDirection vertical
]
