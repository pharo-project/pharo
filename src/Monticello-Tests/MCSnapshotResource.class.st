Class {
	#name : #MCSnapshotResource,
	#superclass : #TestResource,
	#instVars : [
		'snapshot'
	],
	#category : 'Monticello-Tests'
}

{ #category : #accessing }
MCSnapshotResource class >> mockPackage [
	^ (MCPackage new name: self mockPackageName)
]

{ #category : #accessing }
MCSnapshotResource class >> mockPackageName [
	^ 'MonticelloMocks'
]

{ #category : #accessing }
MCSnapshotResource class >> takeSnapshot [
	^ self mockPackage snapshot
]

{ #category : #accessing }
MCSnapshotResource >> definitions [
	^ snapshot definitions
]

{ #category : #running }
MCSnapshotResource >> setUp [
	snapshot := self class takeSnapshot.
]

{ #category : #accessing }
MCSnapshotResource >> snapshot [
	^ snapshot
]
