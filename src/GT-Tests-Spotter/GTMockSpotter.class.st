Class {
	#name : #GTMockSpotter,
	#superclass : #GTSpotter,
	#category : #'GT-Tests-Spotter-Scripting-Mocks'
}

{ #category : #'accessing-defaults' }
GTMockSpotter >> defaultPagerModel [
	^ GLMMockPagerModel new
]

{ #category : #'accessing-defaults' }
GTMockSpotter >> defaultStep [
	^ GTMockSpotterStep new
]
