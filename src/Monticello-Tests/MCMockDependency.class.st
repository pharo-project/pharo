Class {
	#name : #MCMockDependency,
	#superclass : #Object,
	#instVars : [
		'name',
		'children',
		'hasResolution'
	],
	#category : #Monticello-Tests
}

{ #category : #'instance creation' }
MCMockDependency class >> fromTree: anArray [ 
	^ self new initializeWithTree: anArray
]

{ #category : #comparing }
MCMockDependency >> = other [
	^ self name = other name
]

{ #category : #accessing }
MCMockDependency >> children [
	^ children collect: [:ea | self class fromTree: ea]
]

{ #category : #resolving }
MCMockDependency >> hasResolution [
	^ hasResolution
]

{ #category : #comparing }
MCMockDependency >> hash [
	^ self name hash
]

{ #category : #accessing }
MCMockDependency >> initializeWithTree: expr [
	expr isSymbol
		ifTrue: [name := expr.
				children := Array new.
				hasResolution := true.]
		ifFalse: [name := expr first.
				expr second isSymbol
					ifTrue: [hasResolution := false.
							children := Array new]
					ifFalse: [hasResolution := true.
							children := expr second]]
]

{ #category : #mocks }
MCMockDependency >> mockVersionInfo [
	^ MCVersionInfo
		name: self name
		id: (self uuidForName: name)
		message: ''
		date: nil
		time: nil
		author: ''
		ancestors: #()
]

{ #category : #accessing }
MCMockDependency >> name [
	^ name
]

{ #category : #resolving }
MCMockDependency >> resolve [
	^ self hasResolution
		ifTrue: [MCVersion new
					setPackage: MCSnapshotResource mockPackage
					info: self mockVersionInfo
					snapshot: MCSnapshotResource current snapshot
					dependencies: self children]
		ifFalse: [nil]
]

{ #category : #mocks }
MCMockDependency >> uuidForName: aName [ 
	| nm id |
	nm := aName asString.
	id := '00000000-0000-0000-0000-0000000000' 
				, (nm size = 1 ifTrue: [nm , '0'] ifFalse: [nm]).
	^UUID fromString: id
]
