Class {
	#name : #GTSpotterStepFilteringTest,
	#superclass : #GTSpotterStepTest,
	#category : #'GT-Tests-Spotter-Scripting'
}

{ #category : #running }
GTSpotterStepFilteringTest >> setUp [
	super setUp.
	
	"Reser the state of spotter as the filter nees to be set before running a query."
	spotter := GTMockSpotter new.
	currentStep := currentProcessor := currentCandidate := nil
]

{ #category : #tests }
GTSpotterStepFilteringTest >> testProcessorsFiltering [

	spotter filterByMethods: {
		GTSpotter>>#spotterImplementorsFor:.
		GTSpotter>>#spotterForPragmasFor:}.

	self shouldnt: [ spotter setText: '' from: self ] raise: Error. 
	
	self assertSearch: ' '.
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorByTitle: 'Implementors'.

	self assert: currentStep processors size equals: 2.
	
]
