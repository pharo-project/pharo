"
I represent a trategy to map nodes and links of a ruler to colors. I'm used by the HiSimpleRenderer.
"
Class {
	#name : #HiAbstractColorStrategy,
	#superclass : #Object,
	#category : #'Hiedra-UI'
}

{ #category : #accessing }
HiAbstractColorStrategy >> colorForLink: aHiLink [
	^ self subclassResponsibility
]

{ #category : #accessing }
HiAbstractColorStrategy >> colorForNode: aHiNode [
	^ self subclassResponsibility
]

{ #category : #accessing }
HiAbstractColorStrategy >> ruler: aHiRuler [
	self subclassResponsibility
]
