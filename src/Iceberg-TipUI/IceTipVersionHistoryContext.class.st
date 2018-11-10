Class {
	#name : #IceTipVersionHistoryContext,
	#superclass : #IceTipHistoryContext,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipVersionHistoryContext >> selectedCommit [
	^ self item commit
]

{ #category : #accessing }
IceTipVersionHistoryContext >> version [
	^ self item
]
