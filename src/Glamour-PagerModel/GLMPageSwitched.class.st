Class {
	#name : #GLMPageSwitched,
	#superclass : #Announcement,
	#instVars : [
		'smooth',
		'pageIndex'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #accessing }
GLMPageSwitched >> pageIndex [
	^ pageIndex
]

{ #category : #accessing }
GLMPageSwitched >> pageIndex: anObject [
	pageIndex := anObject
]

{ #category : #accessing }
GLMPageSwitched >> smooth [
	^ smooth
]

{ #category : #accessing }
GLMPageSwitched >> smooth: anObject [
	smooth := anObject
]
