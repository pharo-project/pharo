"
I map all nodes to a color and all links to another color (by default the same color).
"
Class {
	#name : #HiUniformColorStrategy,
	#superclass : #HiAbstractColorStrategy,
	#instVars : [
		'linkColor',
		'nodeColor'
	],
	#category : #'Hiedra-UI'
}

{ #category : #accessing }
HiUniformColorStrategy >> colorForLink: aHiLink [
	^ linkColor
]

{ #category : #accessing }
HiUniformColorStrategy >> colorForNode: aHiNode [
	^ nodeColor
]

{ #category : #initialization }
HiUniformColorStrategy >> initialize [
	super initialize.
	linkColor := nodeColor := Smalltalk ui theme caretColor
]

{ #category : #accessing }
HiUniformColorStrategy >> linkColor [
	^ linkColor
]

{ #category : #accessing }
HiUniformColorStrategy >> linkColor: anObject [
	linkColor := anObject
]

{ #category : #accessing }
HiUniformColorStrategy >> nodeColor [
	^ nodeColor
]

{ #category : #accessing }
HiUniformColorStrategy >> nodeColor: anObject [
	^ nodeColor := anObject
]

{ #category : #accessing }
HiUniformColorStrategy >> ruler: aHiRuler [
	"Nothing to do"
]
