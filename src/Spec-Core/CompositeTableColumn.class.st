"
I'm a composite column.
I contain other column definitions to compose a single type of column. 
Typically, I will compose images and texts (or checkbox and text, as for do a cherry pick).
"
Class {
	#name : #CompositeTableColumn,
	#superclass : #TableColumn,
	#instVars : [
		'columns'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #'instance creation' }
CompositeTableColumn class >> title: aString withAll: aCollection [

	^ self new
		title: aString;
		addAllColumns: aCollection;
		yourself
]

{ #category : #'instance creation' }
CompositeTableColumn class >> with: column [

	^ self new
		addColumn: column;
		yourself
]

{ #category : #'instance creation' }
CompositeTableColumn class >> with: column1 with: column2 [ 

	^ self new
		addColumn: column1;
		addColumn: column2;
		yourself
]

{ #category : #'instance creation' }
CompositeTableColumn class >> with: column1 with: column2 with: column3 [

	^ self new
		addColumn: column1;
		addColumn: column2;
		addColumn: column3;
		yourself
]

{ #category : #'instance creation' }
CompositeTableColumn class >> withAll: aCollection [

	^ self new
		addAllColumns: aCollection;
		yourself
]

{ #category : #visiting }
CompositeTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitCompositeColumn: self
]

{ #category : #accessing }
CompositeTableColumn >> addAllColumns: aCollection [ 
	
	aCollection do: [ :each | self addColumn: each ]
]

{ #category : #accessing }
CompositeTableColumn >> addColumn: aTableColumn [ 
	
	columns add: aTableColumn
]

{ #category : #accessing }
CompositeTableColumn >> columns [

	^ columns
]

{ #category : #initialization }
CompositeTableColumn >> initialize [
	
	super initialize.
	"there will be at least two, if it is composed :)"
	columns := OrderedCollection new: 2
]

{ #category : #testing }
CompositeTableColumn >> isComposite [

	^ true
]
