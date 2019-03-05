"
I a base definition for table columns.
I define common behavior of a column: 

- title.
- evaluation. A table will handle objects. This is a block to transform that object into the output of the column.
- width. optional width of a column
"
Class {
	#name : #TableColumn,
	#superclass : #Object,
	#instVars : [
		'title',
		'evaluation',
		'width'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #'instance creation' }
TableColumn class >> evaluated: aValuable [

	^ self new 
		evaluated: aValuable;
		yourself
]

{ #category : #'instance creation' }
TableColumn class >> title: aString [

	^ self new 
		title: aString;
		yourself
]

{ #category : #'instance creation' }
TableColumn class >> title: aString evaluated: aValuable [

	^ self new 
		title: aString;
		evaluated: aValuable;
		yourself
]

{ #category : #visiting }
TableColumn >> acceptColumnVisitor: aBuilder [

	^ self subclassResponsibility
]

{ #category : #accessing }
TableColumn >> evaluated: aValuable [

	evaluation := aValuable
]

{ #category : #accessing }
TableColumn >> evaluation [

	^ evaluation
]

{ #category : #accessing }
TableColumn >> evaluation: anObject [

	evaluation := anObject
]

{ #category : #testing }
TableColumn >> isEditable [

	^ false
]

{ #category : #accessing }
TableColumn >> readObject: anObject [ 

	^ self evaluation value: anObject
]

{ #category : #accessing }
TableColumn >> title [
	^ title
]

{ #category : #accessing }
TableColumn >> title: anObject [
	title := anObject
]

{ #category : #accessing }
TableColumn >> width [
	^ width
]

{ #category : #accessing }
TableColumn >> width: anObject [
	width := anObject
]
