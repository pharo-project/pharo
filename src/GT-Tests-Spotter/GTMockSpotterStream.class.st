Class {
	#name : #GTMockSpotterStream,
	#superclass : #GTSpotterUIThreadStream,
	#category : #'GT-Tests-Spotter-Scripting-Mocks'
}

{ #category : #performing }
GTMockSpotterStream >> performSymbol: aSymbol withArguments: aCollection [
	self receiver perform: aSymbol withEnoughArguments: aCollection
]
