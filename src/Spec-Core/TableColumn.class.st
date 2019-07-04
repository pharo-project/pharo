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
		'expandable',
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
TableColumn >> beExpandable [

	expandable := true
]

{ #category : #accessing }
TableColumn >> beNotExpandable [

	expandable := false
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

{ #category : #initialization }
TableColumn >> initialize [

	super initialize.
	self beExpandable
]

{ #category : #testing }
TableColumn >> isComposite [

	^ false
]

{ #category : #testing }
TableColumn >> isEditable [

	^ false
]

{ #category : #testing }
TableColumn >> isExpandable [

	"if width is defined, it cannot be expandable (because column with take the defined width)"
	^ expandable and: [ self width isNil ]
]

{ #category : #accessing }
TableColumn >> readObject: anObject [ 

	^ self evaluation cull: anObject
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
