"
I am an abstract class providing all the properties shared amongs all the morphic specific adapters
"
Class {
	#name : #AbstractMorphicAdapter,
	#superclass : #AbstractAdapter,
	#category : #'Spec-MorphicAdapters'
}

{ #category : #specs }
AbstractMorphicAdapter class >> defaultSpec [

	^ SpecLayout composed
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> acceptDropBlock [

	^ self model acceptDropBlock
]

{ #category : #'drag and drop - private' }
AbstractMorphicAdapter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [

	^ self acceptDropBlock cull: draggedMorph model model cull: event cull: source model model
]

{ #category : #protocol }
AbstractMorphicAdapter >> add: aWidget [
	
	self widgetDo: [ :w | w ensureLayoutAndAddMorph: aWidget asWidget ]
]

{ #category : #dispatch }
AbstractMorphicAdapter >> beginsWith: aString fromList: aMorph [
	"This method is used bu the list for the search of elements when you are typing directly in the list"
	
	^ false
]

{ #category : #'protocol-shortcuts' }
AbstractMorphicAdapter >> bindKeyCombination: aShortcut toAction: aBlock [

	self widget ifNotNil: [ :w | w bindKeyCombination: aShortcut toAction: aBlock ]
]

{ #category : #'protocol-shortcuts' }
AbstractMorphicAdapter >> bindMenuKeyCombination: aShortcut toAction: aBlock [

	self widget ifNotNil: [ :w | w bindKeyCombination: aShortcut toAction: aBlock ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> borderColor [

	^ self model borderColor
]

{ #category : #protocol }
AbstractMorphicAdapter >> borderColor: color [

	self widget ifNotNil: [ :w | w borderColor: color ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> borderWidth [

	^ self model borderWidth
]

{ #category : #protocol }
AbstractMorphicAdapter >> borderWidth: width [

	self widget ifNotNil: [ :w | w borderWidth: width ]
]

{ #category : #'spec protocol' }
AbstractMorphicAdapter >> changed [

	^ self widgetDo: [ :w | w changed ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> color [

	^ self model color
]

{ #category : #protocol }
AbstractMorphicAdapter >> color: color [

	self widgetDo: [ :w | w color: color ]
]

{ #category : #'spec protocol' }
AbstractMorphicAdapter >> delete [
	
	self widgetDo: [ :w | w delete ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> disable [
	
	self model enabled: false
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> dragEnabled [

	^ self model dragEnabled
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> dragEnabled: aBoolean [

	self widget ifNotNil: [ :w | w dragEnabled: aBoolean ]
]

{ #category : #'drag and drop - private' }
AbstractMorphicAdapter >> dragPassengerFor: item inMorph: dragSource [ 
	
	^ self dragTransformationBlock cull: item cull: dragSource
]

{ #category : #'drag and drop - private' }
AbstractMorphicAdapter >> dragPassengersFor: item inMorph: dragSource [ 
	
	^ { self dragTransformationBlock cull: item cull: dragSource }
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> dragTransformationBlock [

	^ self model dragTransformationBlock
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> dropEnabled [

	^ self model dropEnabled
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> dropEnabled: aBoolean [

	self widget ifNotNil: [ :w | w dropEnabled: aBoolean ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> enable [

	self model enabled: true
]

{ #category : #protocol }
AbstractMorphicAdapter >> enabled [
	
	^ self model enabled
]

{ #category : #protocol }
AbstractMorphicAdapter >> enabled: aBoolean [

	self widget ifNotNil: [ :w | w enabled: aBoolean ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> extent [

	^ self widget extent
]

{ #category : #protocol }
AbstractMorphicAdapter >> hRigid [
	
	self widgetDo: [ :w | w hResizing: #rigid ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> hShrinkWrap [
	
	self widgetDo: [ :w | w hResizing: #shrinkWrap ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> hSpaceFill [
	
	self widgetDo: [ :w | w hResizing: #spaceFill ]
]

{ #category : #dispatch }
AbstractMorphicAdapter >> heightToDisplayInList: aList [
	"Return the width of my representation as a list item"
	
	self model extent ifNotNil: [:ex | ^ ex y ].
	self model initialExtent ifNotNil: [:ex | ^ ex y ].
	
	self widget ifNil: [ self buildWithSpec ].
	self widget 
		vResizing: #rigid;
		hResizing: #rigid.
		
	^ self widget heightToDisplayInList: aList
]

{ #category : #dispatch }
AbstractMorphicAdapter >> heightToDisplayInTree: aTree [
	"Return the width of my representation as a list item"
	
	self extent ifNotNil: [:ex | ^ ex y ].
	self initialExtent ifNotNil: [:ex | ^ ex y ].
	
	self widget ifNil: [ self buildWithSpec ].
	^ self widget heightToDisplayInTree: aTree
]

{ #category : #protocol }
AbstractMorphicAdapter >> help [

	^ self model help
]

{ #category : #testing }
AbstractMorphicAdapter >> isMorphicAdapter [

	^ true
]

{ #category : #protocol }
AbstractMorphicAdapter >> layout: aLayout [
	| layout |
	
	layout := aLayout asMorphicLayout.

	self widgetDo: [ :w | w layoutFrame: layout ]
]

{ #category : #dispatch }
AbstractMorphicAdapter >> listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph [
	"Specify how this object as a list item should be drawn"

	self widget ifNil: [ self buildWithSpec ].
	self widget 
		vResizing: #rigid;
		hResizing: #rigid.
		
	self widget
		listRenderOn: aCanvas
		atRow: aRow
		bounds: drawBounds
		color: drawColor
		backgroundColor: backgroundColor
		from: aMorph
]

{ #category : #'protocol-shortcuts' }
AbstractMorphicAdapter >> removeKeyCombination: aShortcut [

	self widget ifNotNil: [ :w | w removeKeyCombination: aShortcut ]
]

{ #category : #'protocol-shortcuts' }
AbstractMorphicAdapter >> removeMenuKeyCombination: aShortcut [

	self widget ifNotNil: [ :w | w removeKeyCombination: aShortcut ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> removeSubWidgets [
	
	self widgetDo: [ :w | w removeAllMorphs ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> setBalloonText: aString [

	self widget ifNotNil: [ :w | w setBalloonText: aString ]
]

{ #category : #'spec protocol' }
AbstractMorphicAdapter >> setModal: aWindow [ 

	self widgetDo: [ :w | w setModal: aWindow ]
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> transferBlock [

	^ self model transferBlock
]

{ #category : #'drag and drop - private' }
AbstractMorphicAdapter >> transferFor: passenger from: source [
	^ (self transferBlock cull: passenger cull: self model) buildWithSpec
]

{ #category : #dispatch }
AbstractMorphicAdapter >> treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph [
	"Specify how this object as a list item should be drawn"

	self widget ifNil: [ self buildWithSpec ].
	self widget treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph
]

{ #category : #protocol }
AbstractMorphicAdapter >> useProportionalLayout [

	self widgetDo: [ :w | w changeProportionalLayout ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> vRigid [
	
	self widgetDo: [ :w | w vResizing: #rigid ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> vShrinkWrap [
	
	self widgetDo: [ :w | w vResizing: #shrinkWrap ]
]

{ #category : #protocol }
AbstractMorphicAdapter >> vSpaceFill [
	
	self widgetDo: [ :w | w vResizing: #spaceFill ]
]

{ #category : #'drag and drop' }
AbstractMorphicAdapter >> wantDropBlock [

	^ self model wantDropBlock
]

{ #category : #'drag and drop - private' }
AbstractMorphicAdapter >> wantsDroppedMorph: draggedMorph event: anEvent inMorph: source [

	^ self wantDropBlock cull: draggedMorph cull: anEvent cull:source
]

{ #category : #protocol }
AbstractMorphicAdapter >> when: anAnnouncement do: aBlock [

	self widgetDo: [ :w | w announcer
		when: anAnnouncement do: aBlock ]
]

{ #category : #dispatch }
AbstractMorphicAdapter >> widthToDisplayInList: aList [
	"Return the width of my representation as a list item"
	
	self model extent ifNotNil: [:ex | ^ ex x ].
	self model initialExtent ifNotNil: [:ex | ^ ex x ].

	self widget ifNil: [ self buildWithSpec ].
	self widget 
		vResizing: #rigid;
		hResizing: #rigid.
		
	^ self widget widthToDisplayInList: aList
]

{ #category : #dispatch }
AbstractMorphicAdapter >> widthToDisplayInTree: aTree [
	"Return the width of my representation as a list item"
	
	self extent ifNotNil: [:ex | ^ ex x ].
	self initialExtent ifNotNil: [:ex | ^ ex x ].
	
	self widget ifNil: [ self buildWithSpec ].
	^ self widget widthToDisplayInTree: aTree
]
