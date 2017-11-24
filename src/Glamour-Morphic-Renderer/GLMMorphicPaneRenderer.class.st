Class {
	#name : #GLMMorphicPaneRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'container',
		'contentMorph'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicPaneRenderer >> actOnMatchingPresentationChanged: anAnnouncement [
	container removeAllMorphs.
	self renderContainerFor: anAnnouncement pane.
	self renderer window
		ifNotNil: [ 
			self flag: 'this check is fishy'.
			self renderer window fullBounds ]
]

{ #category : #rendering }
GLMMorphicPaneRenderer >> actOnPresentationToolbarUpdated: anAnnouncement [
	"If this method is called contentMorph should be an tab widget"
	| presentation |
	
	presentation := anAnnouncement presentation.	
	contentMorph updatePageAtIndex: 1
			withLabel: (self titleOrIconOf: presentation in: contentMorph)
			toolbar: (self renderToolbarForPresentation: presentation inMorph: contentMorph)
			action: (self labelActionBrickFor: presentation)
	
	
]

{ #category : #rendering }
GLMMorphicPaneRenderer >> render: aPane [
	container := GLMMorphic containerMorph.
	self renderContainerFor: aPane.
	aPane when: GLMMatchingPresentationsChanged send: #actOnMatchingPresentationChanged: to: self.
	(aPane presentations hasTitle or: [aPane presentations hasTitleIcon]) ifTrue: [
		aPane when: GLMPresentationToolbarUpdated send: #actOnPresentationToolbarUpdated: to: self. ].
	^ container
]

{ #category : #rendering }
GLMMorphicPaneRenderer >> renderContainerFor: aPane [

	contentMorph := self 
		morph: (self renderObject: aPane presentations) 
		withTitleOrNilOf: aPane presentations.
		
	container
		addMorph: contentMorph fullFrame: (LayoutFrame identity);
		"adoptPaneColor: Color transparent"
		yourself
]
