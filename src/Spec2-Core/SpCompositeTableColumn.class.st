"
A composite column (a column that joins other colums to form a single cell view).
A `SpCompositeTableColumn` contain other column definitions to compose a single type of column. 
Typically, it will compose images and texts (or checkbox and text, as for do a cherry pick).

The user does not needs to set an evaluation block for this column (as its value is taken in the composed columns).


## Example
```
SpCompositeTableColumn new 
	title: 'My string';
	addColumn: (SpCompositeTableColumn new
			title: 'Classes';
			addColumn: (SpImageTableColumn new
				width: 20;
				evaluated: [ :aClass | aClass systemIcon ];
				yourself);
			addColumn: (SpStringTableColumn new
				evaluated: [ :aClass | aClass name ];
				yourself);
			yourself);
	yourself
```
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

{ #category : #api }
SpCompositeTableColumn >> addAllColumns: aCollection [ 
	"Add all columns contained in `aCollection` to the composed columns."
	
	aCollection do: [ :each | self addColumn: each ]
]

{ #category : #api }
SpCompositeTableColumn >> addColumn: aTableColumn [ 
	"Add a column to the composed columns."
	
	columns add: aTableColumn
]

{ #category : #api }
SpCompositeTableColumn >> columns [
	"Answer the columns composing this composite."

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
	"Answer if this is a composite column. 
	 For this class, answer will always be true."

	^ true
]
