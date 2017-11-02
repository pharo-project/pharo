Class {
	#name : #GLMDropListModel,
	#superclass : #Object,
	#instVars : [
		'list',
		'selectionIndex',
		'announcer',
		'glamourPresentation'
	],
	#category : 'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
GLMDropListModel >> announcer [
	^ announcer ifNil: [announcer := GLMAnnouncer new]
]

{ #category : #accessing }
GLMDropListModel >> glamourPresentation [

	^glamourPresentation
]

{ #category : #accessing }
GLMDropListModel >> glamourPresentation: anObject [
	glamourPresentation := anObject
]

{ #category : #accessing }
GLMDropListModel >> list [

	^list ifNil: [ list:= (self glamourPresentation displayValue 
									collect:[ :each| (self glamourPresentation formatedDisplayValueOf: each) asMorph ])]
]

{ #category : #accessing }
GLMDropListModel >> list: anObject [
	list := anObject
]

{ #category : #accessing }
GLMDropListModel >> resetData [

	list := nil.
	selectionIndex := nil
]

{ #category : #accessing }
GLMDropListModel >> selectedItem [
	"Answer the currently selected item or nil if none."

	^self selectionIndex = 0
		ifTrue: [nil]
		ifFalse: [self glamourPresentation displayValue at: self selectionIndex]
]

{ #category : #accessing }
GLMDropListModel >> selectionIndex [

	^ selectionIndex ifNil:[ selectionIndex:= self glamourPresentation selectedIndex ]
]

{ #category : #accessing }
GLMDropListModel >> selectionIndex: anObject [
	selectionIndex := anObject.
	self announcer announce: (
			GLMDropDownListMorphSelectionChanged new 
				selectionValue: self selectedItem;
				selectionIndex: selectionIndex;
				yourself)
]
