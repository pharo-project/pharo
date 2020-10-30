"
A base definition for table columns.
A `SpTableColumn` define the common behavior of a column.
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

{ #category : #adding }
SpTableColumn class >> addDocumentSectionHierarchy: aBuilder [
	
	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Hierarchy' ] withLevel: 2.
	SpDocumentHierarchyBuilder new
		fromClass: SpTableColumn;
		builder: aBuilder;
		filter: [ :eachClass | eachClass package packageName beginsWith: 'Spec2-' ];
		buildFor: self
]

{ #category : #documentation }
SpTableColumn class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	"Sections"
	self documentSections keysAndValuesDo: [ :label :methods |
		SpAbstractWidgetPresenter 
			addDocumentSection: aBuilder 
			label: label 
			methods: methods ].
	"Hierarchy"
	self addDocumentSectionHierarchy: aBuilder.

]

{ #category : #documentation }
SpTableColumn class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
	 }
]

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

{ #category : #api }
SpTableColumn >> beExpandable [
	"Indicate that this column may be expanded if requested by the rendering.
	 This will cause the column to take as much width as it is given.
	 By default, all columns are expandable."

	expandable := true
]

{ #category : #api }
SpTableColumn >> beNotExpandable [
	"Indicate that this column will not be expanded when requested by the rendering.
	 This will cause the column to have a fixed width (often calculated by the width of its 
	 elements)."

	expandable := false
]

{ #category : #api }
SpTableColumn >> evaluated: aBlock [
	"Define how the column will evaluate the element of the table/tree model 
	 (See `SpAbstractListPresenter>>#items:) to get the value to display in this 
	 column.
	 `aBlock` receives one orgument (the element to evaluate) and it needs to answer an 
	 object of the type expected by the type of the column (See SpTableColumn subclasses)"

	evaluation := aBlock
]

{ #category : #api }
SpTableColumn >> evaluation [
	"Answer the evaluation block to transform the table element (See `SpAbstractListPresenter>>#items:`)
	 into a value to display in this column"

	^ evaluation
]

{ #category : #initialization }
SpTableColumn >> initialize [

	super initialize.
	self beExpandable
]

{ #category : #testing }
SpTableColumn >> isComposite [
	"Answer if this is a composite column"

	^ false
]

{ #category : #testing }
SpTableColumn >> isEditable [
	"Answer if this column is editable"

	^ false
]

{ #category : #testing }
SpTableColumn >> isExpandable [
	"Answer if this column is expandable"

	"if width is defined, it cannot be expandable (because column with take the defined width)"
	^ expandable and: [ self width isNil ]
]

{ #category : #testing }
SpTableColumn >> isSortable [
	"Answer if this column is sortable"

	^ self sortFunction notNil
]

{ #category : #private }
SpTableColumn >> readObject: anObject [ 

	^ self evaluation cull: anObject
]

{ #category : #api }
SpTableColumn >> sortFunction [
	"Answer the a sort function to apply to the values of this column in order to sort elements. 
	 If not defined, most table column types will not allow the table sorting capabilities. 
	 This is not the case of `SpStringTableColumn` which defines a sorting by default (but you can 
	 disable it anyway, see `SpStringTableColumn>>#beNotSortable`."

	^ sortFunction
]

{ #category : #api }
SpTableColumn >> sortFunction: aBlockOrSortFunction [
	"Set the sort function to apply to the values of this column in order to sort elements.
	 `aBlockOrSortFunction` is a block that receives two arguments to compare or an instace of 
	 `SortFunction`. "

	sortFunction := aBlockOrSortFunction
]

{ #category : #api }
SpTableColumn >> title [
	"Answer the header title of this column."

	^ title
]

{ #category : #api }
SpTableColumn >> title: aString [
	"Set the header title of this column."

	title := aString
]

{ #category : #api }
SpTableColumn >> width [
	"Answer the fixed width of this column (or nil if column is expandable, 
	 see `SpTableColumn>>#beExpandable`)"

	^ width
]

{ #category : #api }
SpTableColumn >> width: aNumber [
	"Set the width of a column to `aNumber`. 
	 Note that setting width to a value will make this column not expandable. 
	 (See `SpTableColumn>>#beNotExpandable`"

	width := aNumber
]
