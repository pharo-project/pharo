"
I am a project spec specific to metadataless projects (filetree/tonel).
"
Class {
	#name : #MetacelloCypressBaselineProjectSpec,
	#superclass : #MetacelloMCBaselineOfProjectSpec,
	#category : #'Metacello-Cypress-Specs'
}

{ #category : #merging }
MetacelloCypressBaselineProjectSpec >> validateMergeForSpec: aSpec [
	"This method is needed because without it, it is not possible to use a MetacelloCypressBaselineProject kind of project and copy a dependency definition via #project:copyFrom:with:.
	
	Do not remove if you don't really know what you are doing!"
]
