Class {
	#name : #GLMPagerBoundsChanged,
	#superclass : #Announcement,
	#instVars : [
		'bounds'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #accessing }
GLMPagerBoundsChanged >> bounds [
	^ bounds
]

{ #category : #accessing }
GLMPagerBoundsChanged >> bounds: anObject [
	bounds := anObject
]
