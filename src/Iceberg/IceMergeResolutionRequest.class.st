Class {
	#name : #IceMergeResolutionRequest,
	#superclass : #Notification,
	#instVars : [
		'merge'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #exceptiondescription }
IceMergeResolutionRequest >> defaultAction [

	| window mergeMorph |
	mergeMorph := (IceTipMergeMorph forMerger: merge).

	mergeMorph
		fromDescription: merge sourceDescription;
		toDescription: merge targetDescription.

	window := mergeMorph newWindow
		title: 'Merge';
		yourself.

	UIManager default openModal: window.
	^ mergeMorph merged
]

{ #category : #accessing }
IceMergeResolutionRequest >> merge [
	^ merge
]

{ #category : #accessing }
IceMergeResolutionRequest >> merger: anIceMerge [ 
	
	merge := anIceMerge
]
