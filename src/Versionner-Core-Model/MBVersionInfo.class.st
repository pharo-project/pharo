"
A version info
"
Class {
	#name : #MBVersionInfo,
	#superclass : #MBAbstractVersionInfo,
	#category : #'Versionner-Core-Model'
}

{ #category : #accessing }
MBVersionInfo class >> helpLabel [

	^'Version'
]

{ #category : #commands }
MBVersionInfo >> cmdReleaseDevelopmentVersion [
	<command>
	^ VSReleaseDevelopmentVersionCommand
]

{ #category : #commands }
MBVersionInfo >> cmdSetDescription [
	<command>
	^ VSSetDescriptionCommand
]

{ #category : #printing }
MBVersionInfo >> printOn: aStream [
	super printOn: aStream.
	self isDevelopment 
		ifTrue: [ 
			aStream nextPutAll: ' #development'.
			^self ].
	self isStable
		ifTrue: [ aStream nextPutAll: ' #stable' ].
]
