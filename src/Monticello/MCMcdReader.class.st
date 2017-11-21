"
Reader for an MCD format
"
Class {
	#name : #MCMcdReader,
	#superclass : #MCMczReader,
	#instVars : [
		'baseInfo',
		'patch'
	],
	#category : #Monticello-Storing
}

{ #category : #accessing }
MCMcdReader class >> extension [
	^ 'mcd'
]

{ #category : #accessing }
MCMcdReader >> baseInfo [
	^ baseInfo ifNil: [self loadBaseInfo]
]

{ #category : #accessing }
MCMcdReader >> basicVersion [
	^ MCDiffyVersion
		package: self package
		info: self info
		dependencies: self dependencies
		baseInfo: self baseInfo
		patch: self patch
]

{ #category : #loading }
MCMcdReader >> buildPatchFrom: oldDefinitions to: newDefinitions [
	^ MCPatch
		fromBase: (MCSnapshot fromDefinitions: oldDefinitions)
		target: (MCSnapshot fromDefinitions: newDefinitions)
]

{ #category : #loading }
MCMcdReader >> loadBaseInfo [
	^ baseInfo := self extractInfoFrom: (self parseMember: 'base')
]

{ #category : #loading }
MCMcdReader >> loadPatch [
	| old new |
	(self zip memberNamed: 'patch.bin') ifNotNil:
		[:m | [^ patch := (MCDataStream on: m contentStream) next ]
			on: Error do: [:fallThrough |]].
	definitions := OrderedCollection new.
	(self zip membersMatching: 'old/*')
		do: [:m | self extractDefinitionsFrom: m].
	old := definitions asArray.
	definitions := OrderedCollection new.
	(self zip membersMatching: 'new/*')
		do: [:m | self extractDefinitionsFrom: m].
	new := definitions asArray.
	^ patch := self buildPatchFrom: old to: new.
	
]

{ #category : #accessing }
MCMcdReader >> patch [
	^ patch ifNil: [self loadPatch]
]
