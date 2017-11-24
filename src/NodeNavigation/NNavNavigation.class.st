"
I'm the one who manage the navigation and I have memory from what I've been navigating
"
Class {
	#name : #NNavNavigation,
	#superclass : #Object,
	#instVars : [
		'currentEditor',
		'lastSelection',
		'currentSelection'
	],
	#classVars : [
		'Instance',
		'UseArrowsShortcuts'
	],
	#category : #'NodeNavigation-Core'
}

{ #category : #'instance creation' }
NNavNavigation class >> new [
	"yes I'm a singleton"
	Instance ifNil: [Instance := super new].
	^Instance. 
]

{ #category : #configuration }
NNavNavigation class >> useArrowsShortcuts [
	^UseArrowsShortcuts ifNil: [ false ]

]

{ #category : #configuration }
NNavNavigation class >> useArrowsShortcuts: enable [
	UseArrowsShortcuts := enable.
	KMRepository reset.

]

{ #category : #'current-context' }
NNavNavigation >> bestNodeForSelection [
	| root |
	
	currentEditor isWorkspace 
		ifTrue: [root := RBParser parseFaultyExpression: self currentText]
		ifFalse:  [root := RBParser parseFaultyMethod: self currentText]. 
	^ root bestNodeFor: self textSelection .	
]

{ #category : #'current-context' }
NNavNavigation >> currentText [
	^ self textArea text
]

{ #category : #accessing }
NNavNavigation >> lastSelection [
	^ lastSelection
]

{ #category : #navigation }
NNavNavigation >> navigate: editor direction: direction [
	| newSelection |

	currentEditor  := editor.
	newSelection := direction findSelectionFor: self.
	newSelection ifNil: [ ^self ].
	self textArea selectFrom: newSelection first to: newSelection last.
	self updateState: newSelection .


]

{ #category : #'current-context' }
NNavNavigation >> textArea [
	^ currentEditor sourceTextArea 
]

{ #category : #'current-context' }
NNavNavigation >> textSelection [ 
	^ self textArea selectionInterval
]

{ #category : #private }
NNavNavigation >> updateState: newSelection [

	newSelection = currentSelection ifFalse: [ lastSelection := currentSelection].
	currentSelection := newSelection.
	currentEditor := nil.
	
]
