"
I'm a model for IceCommit entries. 
"
Class {
	#name : #IceTipCommitModel,
	#superclass : #IceTipCommitishModel,
	#traits : 'TIceCopyCommitId',
	#classTraits : 'TIceCopyCommitId classTrait',
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipCommitModel >> ancestorShortId [
	^ self entity ancestors first shortId
]

{ #category : #accessing }
IceTipCommitModel >> ancestorShortIdOrOrigin [
	^ self entity ancestors 
		ifNotEmpty: [ self ancestorShortId ]
		ifEmpty: [ 'Origin' ]
]

{ #category : #accessing }
IceTipCommitModel >> ancestors [
	^ self entity ancestors
]

{ #category : #accessing }
IceTipCommitModel >> author [
	^ self entity author
]

{ #category : #private }
IceTipCommitModel >> calculateDiff: aBlock [
	| diff |
	
	UIManager default 
		informUser: 'Calculating diff'
		during: [ 
			diff := aBlock value.
			diff build ].

	^ diff
]

{ #category : #accessing }
IceTipCommitModel >> comment [
	^ self entity comment
]

{ #category : #accessing }
IceTipCommitModel >> commit [
	
	^ self entity
]

{ #category : #accessing }
IceTipCommitModel >> commitModels [
	| commits |
	commits := OrderedCollection new.
	self entity commitsDo: [ :each | 
		commits add: (IceTipCommitModel 
			repositoryModel: self repositoryModel 
			on: each) 
			beCached ].
	^ commits
]

{ #category : #accessing }
IceTipCommitModel >> datetime [
	^ self entity datetime
]

{ #category : #accessing }
IceTipCommitModel >> descriptionWithDecoration [
	| text |
	text := (', ' join: self entity tagNames) asText allBold makeAllColor: Color gray.
	text ifNotEmpty: [ text := text , ' ' ].
	text append: self entity comment asText.
	^ text
]

{ #category : #accessing }
IceTipCommitModel >> diffFromFirstAncestor [
	| diff ancestorCommit |
	
	ancestorCommit := self entity ancestors 
		ifNotEmpty: #first
		ifEmpty: [ IceNoCommit new ].
	diff := self calculateDiff: [ ancestorCommit diffTo: self entity ].
	^ IceTipCommitDiffModel
		repositoryModel: self repositoryModel 
		on: diff
]

{ #category : #accessing }
IceTipCommitModel >> diffFromHead [
	| head diff |
	
	head := self repositoryModel entity headCommit.
	diff := self calculateDiff: [ head diffTo: self entity ].
	^ IceTipCommitDiffModel
		repositoryModel: self repositoryModel 
		on: diff
]

{ #category : #accessing }
IceTipCommitModel >> diffToFirstAncestor [
	| diff ancestorCommit |
	
	ancestorCommit := self entity ancestors 
		ifNotEmpty: #first
		ifEmpty: [ IceNoCommit new ].
	diff := self calculateDiff: [ self entity diffTo: ancestorCommit ].
	^ IceTipCommitDiffModel
		repositoryModel: self repositoryModel 
		on: diff
]

{ #category : #accessing }
IceTipCommitModel >> diffToHead [
	| head diff |
	
	head := self repositoryModel entity headCommit.
	diff := self calculateDiff: [ self entity diffTo: head ].
	^ IceTipCommitDiffModel
		repositoryModel: self repositoryModel 
		on: diff
]

{ #category : #accessing }
IceTipCommitModel >> diffToWorkingCopy [
	| diff |
	diff := self calculateDiff: [ self entity diffToWorkingCopy ].
	^ IceTipCommitDiffModel
		repositoryModel: self repositoryModel 
		on: diff
]

{ #category : #testing }
IceTipCommitModel >> hasMultipleAncestors [
	
	^ self entity hasMultipleAncestors
]

{ #category : #accessing }
IceTipCommitModel >> id [
	^ self entity id
]

{ #category : #accessing }
IceTipCommitModel >> info [
	^ {('Commit:' -> ('[' , self shortId , '] ' , self id)).
	('Parents:' -> (self entity ancestors collect: #shortId) asCommaString).
	('Author:' -> self entity author).
	('Date:' -> self entity timeStamp asStringYMDHM).
	('Tags:' -> self entity tagNames asCommaString).
	('Comment:' -> self entity comment)}
]

{ #category : #testing }
IceTipCommitModel >> isMerged [
	^ self entity isMerged
]

{ #category : #private }
IceTipCommitModel >> mergeHistoryFromAncestor: anIceGitCommit [ 
	^ (IceTipCommitRangeModel 
		repositoryModel: self repositoryModel 
		on: anIceGitCommit)
		toCommit: self entity parent;
		beCached
]

{ #category : #actions }
IceTipCommitModel >> mergeHistoryFromSecondAncestor [
	<noCache>
	
	^ self mergeHistoryFromAncestor: self entity ancestors second
]

{ #category : #accessing }
IceTipCommitModel >> name [
	
	^ self entity description
]

{ #category : #accessing }
IceTipCommitModel >> shortId [
	^ self entity shortId
]

{ #category : #accessing }
IceTipCommitModel >> timeStamp [
	^ self entity timeStamp
]
