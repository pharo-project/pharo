"
I'm a composite column.
I contain other column definitions to compose a single type of column. 
Typically, I will compose images and texts (or checkbox and text, as for do a cherry pick).
"
Class {
	#name : #SpCompositeTableColumn,
	#superclass : #SpTableColumn,
	#instVars : [
		'columns'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #'instance creation' }
SpCompositeTableColumn class >> title: aString withAll: aCollection [

	^ self new
		title: aString;
		addAllColumns: aCollection;
		yourself
]

{ #category : #'instance creation' }
SpCompositeTableColumn class >> with: column [

	^ self new
		addColumn: column;
		yourself
]

{ #category : #'instance creation' }
SpCompositeTableColumn class >> with: column1 with: column2 [ 

	^ self new
		addColumn: column1;
		addColumn: column2;
		yourself
]

{ #category : #'instance creation' }
SpCompositeTableColumn class >> with: column1 with: column2 with: column3 [

	^ self new
		addColumn: column1;
		addColumn: column2;
		addColumn: column3;
		yourself
]

{ #category : #'instance creation' }
SpCompositeTableColumn class >> withAll: aCollection [

	^ self new
		addAllColumns: aCollection;
		yourself
]

{ #category : #visiting }
SpCompositeTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitCompositeColumn: self
]

{ #category : #accessing }
SpCompositeTableColumn >> addAllColumns: aCollection [ 
	
	aCollection do: [ :each | self addColumn: each ]
]

{ #category : #accessing }
SpCompositeTableColumn >> addColumn: aTableColumn [ 
	
	columns add: aTableColumn
]

{ #category : #accessing }
SpCompositeTableColumn >> columns [

	^ columns
]

{ #category : #initialization }
SpCompositeTableColumn >> initialize [
	
	super initialize.
	"there will be at least two, if it is composed :)"
	columns := OrderedCollection new: 2
]

{ #category : #testing }
SpCompositeTableColumn >> isComposite [

	^ true
]
