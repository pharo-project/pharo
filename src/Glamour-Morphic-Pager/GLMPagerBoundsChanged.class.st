Class {
	#name : #GLMPagerBoundsChanged,
	#superclass : #Announcement,
	#instVars : [
		'bounds'
	],
	#category : #Glamour-Morphic-Pager-Announcement
}

{ #category : #accessing }
GLMPagerBoundsChanged >> bounds [
	^ bounds
]

{ #category : #accessing }
GLMPagerBoundsChanged >> bounds: anObject [
	bounds := anObject
]
