Class {
	#name : #GTMockSpotterStep,
	#superclass : #GTSpotterStep,
	#category : #'GT-Tests-Spotter-Scripting-Mocks'
}

{ #category : #'accessing-defaults' }
GTMockSpotterStep >> defaultStream [
	^ GTMockSpotterStream new
]

{ #category : #'accessing-defaults' }
GTMockSpotterStep >> defaultTask [
	^ GTMockSpotterTask new
]
