"
A MBBaselineInfo holds information on a baselinf of a configuration.


"
Class {
	#name : #MBBaselineInfo,
	#superclass : #MBAbstractVersionInfo,
	#category : #Versionner-Core-Model
}

{ #category : #accessing }
MBBaselineInfo class >> helpLabel [

	^'Baseline'
]

{ #category : #commands }
MBBaselineInfo >> cmdCopyBaseline [
	<command>
	^ MBCopyBaselineCommand
]

{ #category : #printing }
MBBaselineInfo >> printOn: aStream [
	super printOn: aStream.
	self isBleedingEdge 
		ifTrue: [ aStream nextPutAll: ' #bleedingEdge' ].
]
