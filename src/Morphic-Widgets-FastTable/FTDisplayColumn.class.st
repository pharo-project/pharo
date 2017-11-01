"
I'm a container for columns in the process of being shown in container. 
I have display column properties (that might or might not be in column it self).

Originally I'm used to send data outside container that might be used (for example, resizing).
"
Class {
	#name : #FTDisplayColumn,
	#superclass : #Object,
	#instVars : [
		'column',
		'width'
	],
	#category : 'Morphic-Widgets-FastTable'
}

{ #category : #'instance creation' }
FTDisplayColumn class >> column: aColumn width: aWidth [
	^ self new 
		column: aColumn;
		width: aWidth;
		yourself
		
]

{ #category : #accessing }
FTDisplayColumn >> column [
	^ column
]

{ #category : #accessing }
FTDisplayColumn >> column: anObject [
	column := anObject
]

{ #category : #accessing }
FTDisplayColumn >> width [
	^ width
]

{ #category : #accessing }
FTDisplayColumn >> width: anObject [
	width := anObject
]
