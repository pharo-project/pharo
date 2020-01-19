Class {
	#name : #SpAbstractTreeSelectionMode,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#selection => SpObservableSlot',
		'#presenter'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #'accessing method dictionary' }
SpAbstractTreeSelectionMode class >> on: aPresenter [

	^ self new
		presenter: aPresenter;
		yourself
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> clearSelection [

	selection := #()
]

{ #category : #initialization }
SpAbstractTreeSelectionMode >> initialize [
	self class initializeSlots: self.
	selection := #().
	super initialize.

]

{ #category : #testing }
SpAbstractTreeSelectionMode >> isEmpty [
	
	^ selection isEmpty
]

{ #category : #accessing }
SpAbstractTreeSelectionMode >> isMultipleSelection [
	^ false
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> itemNotFoundAction [
	"ignore. we do not change the selection"
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> pathOf: anItem [
	^ (self pathOf: anItem from: presenter roots)
		ifNil: [ 	NotFound signalFor: anItem in: presenter roots ]
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> pathOf: anItem from: aCollection [
    | index |

    index := aCollection indexOf: anItem.
    index > 0 ifTrue: [ ^ { index } ].
    
    aCollection withIndexDo: [ :each :i |
        (self pathOf: anItem from: (presenter childrenFor: each))
            ifNotNil: [ :path | ^ { i }, path ] ].
        
    ^ nil
]

{ #category : #accessing }
SpAbstractTreeSelectionMode >> presenter: aPresenter [
	presenter := aPresenter
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> selectItem: anItem [
	[ self selectPath: (self pathOf: anItem) ]
		on: NotFound
		do: [ self itemNotFoundAction ]
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> selectItems: aListOfItems [
	aListOfItems do: 
		[ :each |
			[ self selectPath: (self pathOf: each) ]
			on: NotFound
			do: [ "ignore. we do not change the selection" ] ]
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> selectPath: aPath [
	"Select a node in the tree by providing a list of indexes.
	Example:
		A
			B
			C
			E
		D
			G
			T
	selectPath( #(1 2) ) will select the node C
	selectPath( #(2 1) ) will select the node G
	selectPath( #(2) ) will select the node D
	"
	
	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> selectPaths: pathArray [
	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractTreeSelectionMode >> selectedItem [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpAbstractTreeSelectionMode >> selectedPaths [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpAbstractTreeSelectionMode >> selectionHolder [

	^ self observablePropertyNamed: #selection
]

{ #category : #transfering }
SpAbstractTreeSelectionMode >> transferSubscriptionsTo: anotherSelectionMode [

	self selectionHolder transferSubscriptionsTo: anotherSelectionMode selectionHolder
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> unselectAll [

	self clearSelection
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> unselectItem: anItem [
	[ self unselectPath: (self pathOf: anItem) ]
	on: NotFound 
	do: [ "ignore. nothing to unselect" ]
]

{ #category : #selecting }
SpAbstractTreeSelectionMode >> unselectPath: aPath [
	self subclassResponsibility
]

{ #category : #'api - events' }
SpAbstractTreeSelectionMode >> whenChangedDo: aBlock [
	
	self property: #selection whenChangedDo: aBlock
]
