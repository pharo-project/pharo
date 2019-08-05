"
I am an abstract class that define the way to render a FastTable related presentation.

Description
--------------------

I manage a FastTable dataSource and a FastTable morph. I define how to build and render them. I try to manage the maximum of the rendering and I let my subclasses define some specialities. 

My subclasses are use by a FastTable related presentation as GLMFastListPresentation or GLMFastTreePresentation. 
I use a FTTableMorph to manage the rendering and a subclass of FTDataSource to manage the data. 

Public API and Key Messages
--------------------

- #render: aPresentation 		This method is the main method of the class It allow to render myself on the presentation.
		
My subclasses should manage: 

- #createDataSourceFrom: aPresentation 		This method have to return  a new dataSource that match the presentation.
		
- #specificTableMorphInitializiation and #specificDataSourceInitializiation 		are two methods  my subclasses can override to add functionalites to the FastTable.

Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	tableModel:		Is a DataSource for Glamour that will hold the data of the presentation.
	tableMorph:		Is a FastTableMorph use to display the presentation.

"
Class {
	#name : #GLMMorphicFTRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'tableModel',
		'tableMorph'
	],
	#category : #'Glamour-FastTable-Morphic-Renderer'
}

{ #category : #callbacks }
GLMMorphicFTRenderer >> actOnContextChanged: anAnnouncement [ 
	anAnnouncement property = #selection ifTrue: [ 
		tableModel announcer 
			suspendAll: self suspendedAnnouncements
			while: [ 
				tableModel explicitSelection: anAnnouncement value ] ]
]

{ #category : #callbacks }
GLMMorphicFTRenderer >> actOnPresentationRefreshRequest: anAnnouncement [

	tableMorph window ifNil: [ ^ self ].
	tableMorph window isActive ifFalse: [ ^ self ].
	(tableMorph window boundsInWorld containsRect: tableMorph boundsInWorld) ifFalse: [ ^ self ].
  " tableModel resetElements."
	tableModel resetDataCache.
	tableMorph refresh
]

{ #category : #callbacks }
GLMMorphicFTRenderer >> actOnUpdatedPresentation: anAnnouncement [
	tableModel resetElements.
	tableMorph refresh
]

{ #category : #adding }
GLMMorphicFTRenderer >> addFastTableIn: container [
	container addMorphBack: tableMorph
]

{ #category : #initialization }
GLMMorphicFTRenderer >> createDataSourceFrom: aPresentation [
	self subclassResponsibility
]

{ #category : #initialization }
GLMMorphicFTRenderer >> createFastTable [
	^ FTTableMorph new
]

{ #category : #private }
GLMMorphicFTRenderer >> dataSourceUpdated: announcement [
	tableModel ifNotNil: [ self unsubscribeDataSource: tableModel ].
	tableModel := announcement newDataSource.
	self initializeAnnouncementForDataSource
]

{ #category : #private }
GLMMorphicFTRenderer >> initializeAnnouncementForDataSource [
	tableModel announcer
		when: GLMTreeMorphSelectionChanged
		do: [ :ann | 
			tableModel glamourPresentation announcer
				suspendAllWhile: [ 
					tableModel glamourPresentation selection: ann selectionValue.
					tableModel glamourPresentation selectionPath: ann selectionPathValue ] ].
	tableModel announcer
		when: GLMTreeMorphStrongSelectionChanged
		do: [ :ann | tableModel glamourPresentation strongSelection: ann strongSelectionValue ].
	tableMorph
		onAnnouncement: FTStrongSelectionChanged send: #strongSelection: to: tableModel;
		onAnnouncement: FTSelectionChanged send: #selection: to: tableModel.
	self installActionsOnModel: tableModel fromPresentation: tableModel glamourPresentation.
	self installKeystrokeActionsOnMorph: tableMorph fromPresentation: tableModel glamourPresentation.

]

{ #category : #private }
GLMMorphicFTRenderer >> initializeAnnouncementForPresentation: aPresentation [
	aPresentation when: GLMDataSourceUpdated send: #dataSourceUpdated: to: self.
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMPresentationUpdated send: #actOnUpdatedPresentation: to: self.
	aPresentation when: GLMPresentationRefreshRequest send: #actOnPresentationRefreshRequest: to: self
]

{ #category : #initialization }
GLMMorphicFTRenderer >> initializeTableModel: aPresentation [
	tableModel := (self createDataSourceFrom: aPresentation)
		glamourPresentation: aPresentation;
		yourself.
	self specificDataSourceInitializiation
]

{ #category : #initialization }
GLMMorphicFTRenderer >> initializeTableMorph [
	| presentation |
	presentation := tableModel glamourPresentation.
	tableMorph := self createFastTable.
	tableMorph
		dataSource: tableModel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cornerStyle: tableMorph preferredCornerStyle;
		borderStyle: (BorderStyle inset width: 0);
		dragEnabled: presentation allowsItemDrag;
		dropEnabled: presentation isDropTarget;
		allowsDeselection: presentation allowsDeselection.
	self setFastTableLayoutFrame: tableMorph.
	presentation isMultiple
		ifTrue: [ tableMorph beMultipleSelection ]
		ifFalse: [ tableMorph beSingleSelection ].
	self specificTableMorphInitializiation
]

{ #category : #rendering }
GLMMorphicFTRenderer >> render: aPresentation [
	| container |
	container := GLMMorphic containerMorph.
	self initializeAnnouncementForPresentation: aPresentation.
	self initializeTableModel: aPresentation.
	self initializeTableMorph.
	self addFastTableIn: container.
	aPresentation selection ifNotNil: [ tableModel announcer suspendAll: self suspendedAnnouncements while: [ tableModel explicitSelection: aPresentation selection ] ].
	"No need to initialize the announcements here as setting the data source in a FTTableMorph will do it."
	"self initializeAnnouncementForDataSource."
	^ container
]

{ #category : #'build ui' }
GLMMorphicFTRenderer >> setFastTableLayoutFrame: newTableMorph [
	newTableMorph layoutFrame: LayoutFrame identity
]

{ #category : #initialization }
GLMMorphicFTRenderer >> specificDataSourceInitializiation [
	"Override me if you need to add something to the data source."
]

{ #category : #private }
GLMMorphicFTRenderer >> specificTableMorphColumnsInitialization [
	| presentation |
	presentation := tableModel glamourPresentation.
	tableMorph intercellSpacing: tableModel glamourPresentation intercellSpacing.
	tableMorph beResizable.
	presentation shouldShowHeader 
		ifFalse: [ tableMorph hideColumnHeaders ]
		ifTrue: [ tableMorph showColumnHeaders ].
	tableMorph
		columns:
			(presentation columns
				collect:
					[ :each | 
					GLMFastTableColumn new
						id: (presentation titleValueOfColumn: each);
						width: each width;
						glamourColumn: each;
						target: tableModel actionSelector: #sortByColumn: arguments: {each};
						yourself ]).
]

{ #category : #initialization }
GLMMorphicFTRenderer >> specificTableMorphInitializiation [
	"Override me if need to add something to the tableMorph."

	| presentation |
	presentation := tableModel glamourPresentation.
	tableMorph rowHeight: presentation rowHeight.
	"I set the right function."
	presentation isSearchable
		ifTrue: [ tableMorph enableSearch ].
	presentation isFilterable
		ifTrue: [ presentation needActionButton
			ifTrue: [ tableMorph 
				enableFilterWithAction: presentation actionBlock named: presentation actionName ]
			ifFalse: [ 
				presentation hasFilterBlock 
					ifTrue: [ tableMorph enableFilter: (GLMFilterBlock block: presentation filterBlock) ]
					ifFalse: [ tableMorph enableFilter ] ] ].
	(presentation isFilterable or: [ presentation isSearchable ])
		ifFalse: [ tableMorph disableFunction ]
		ifTrue: [ tableMorph explicitFunction ]
]

{ #category : #private }
GLMMorphicFTRenderer >> suspendedAnnouncements [
	^ OrderedCollection  
		with: GLMTreeMorphSelectionChanged 
		with: GLMTreeMorphStrongSelectionChanged 
		with: GLMMenuItemSelected 
		with: GLMKeyStroke
]

{ #category : #private }
GLMMorphicFTRenderer >> unsubscribeDataSource: aTableModel [
	aTableModel announcer unsubscribe: self.
	tableMorph announcer unsubscribe: aTableModel
]
