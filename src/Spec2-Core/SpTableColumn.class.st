"
I a base definition for table columns.
I define common behavior of a column: 

- title.
- evaluation. A table will handle objects. This is a block to transform that object into the output of the column.
- width. optional width of a column
"
Class {
	#name : #SpTableColumn,
	#superclass : #Object,
	#instVars : [
		'title',
		'evaluation',
		'expandable',
		'width',
		'sortFunction'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #'instance creation' }
SpTableColumn class >> evaluated: aValuable [

	^ self new 
		evaluated: aValuable;
		yourself
]

{ #category : #'instance creation' }
SpTableColumn class >> title: aString [

	^ self new 
		title: aString;
		yourself
]

{ #category : #'instance creation' }
SpTableColumn class >> title: aString evaluated: aValuable [

	^ self new 
		title: aString;
		evaluated: aValuable;
		yourself
]

{ #category : #visiting }
SpTableColumn >> acceptColumnVisitor: aBuilder [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpTableColumn >> beExpandable [

	expandable := true
]

{ #category : #accessing }
SpTableColumn >> beNotExpandable [

	expandable := false
]

{ #category : #accessing }
SpTableColumn >> evaluated: aValuable [

	evaluation := aValuable
]

{ #category : #accessing }
SpTableColumn >> evaluation [

	^ evaluation
]

{ #category : #accessing }
SpTableColumn >> evaluation: anObject [

	evaluation := anObject
]

{ #category : #initialization }
SpTableColumn >> initialize [

	super initialize.
	self beExpandable
]

{ #category : #testing }
SpTableColumn >> isComposite [

	^ false
]

{ #category : #testing }
SpTableColumn >> isEditable [

	^ false
]

{ #category : #testing }
SpTableColumn >> isExpandable [

	"if width is defined, it cannot be expandable (because column with take the defined width)"
	^ expandable and: [ self width isNil ]
]

{ #category : #testing }
SpTableColumn >> isSortable [

	^ self sortFunction notNil
]

{ #category : #accessing }
SpTableColumn >> readObject: anObject [ 

	^ self evaluation cull: anObject
]

{ #category : #accessing }
SpTableColumn >> sortFunction [

	^ sortFunction
]

{ #category : #accessing }
SpTableColumn >> sortFunction: aBlock [

	sortFunction := aBlock
]

{ #category : #accessing }
SpTableColumn >> title [
	^ title
]

{ #category : #accessing }
SpTableColumn >> title: anObject [
	title := anObject
]

{ #category : #accessing }
SpTableColumn >> width [
	^ width
]

{ #category : #accessing }
SpTableColumn >> width: anObject [
	width := anObject
]
