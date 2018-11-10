"
I represent the direction of calculus of diffs.
Since orders may change for different operations, I need a different approach for each scenario.

"
Class {
	#name : #IceTipHistoryDiffUpdater,
	#superclass : #Object,
	#instVars : [
		'tab'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #'instance creation' }
IceTipHistoryDiffUpdater class >> tab: aTab [
	^ self new
		tab: aTab;
		yourself
]

{ #category : #accessing }
IceTipHistoryDiffUpdater >> diffFor: aCommitModel [
	self subclassResponsibility
]

{ #category : #accessing }
IceTipHistoryDiffUpdater >> labelFor: aCommitModel [
	^ self subclassResponsibility
]

{ #category : #'private factory' }
IceTipHistoryDiffUpdater >> newDiffPanelFor: aDiffModel [
	^ IceTipDiffPanel on: aDiffModel
]

{ #category : #accessing }
IceTipHistoryDiffUpdater >> tab [
	^ tab
]

{ #category : #accessing }
IceTipHistoryDiffUpdater >> tab: anObject [
	tab := anObject
]

{ #category : #updating }
IceTipHistoryDiffUpdater >> updateContentsOn: aCommitModel [
	aCommitModel ifNil: [ ^ self ].
	self tab contents: (self newDiffPanelFor: (self diffFor: aCommitModel)) buildWithSpec
]

{ #category : #updating }
IceTipHistoryDiffUpdater >> updateLabelOn: aCommitModel [
	self tab label: (self labelFor: aCommitModel)
]

{ #category : #updating }
IceTipHistoryDiffUpdater >> updateOn: aCommitModel [
	self updateLabelOn: aCommitModel.
	self updateContentsOn: aCommitModel
]
