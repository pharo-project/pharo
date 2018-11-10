"
I'm a context for the history browser (IceHistoryBrowser)
"
Class {
	#name : #IceTipHistoryContext,
	#superclass : #IceTipContext,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipHistoryContext >> item [
	^ item
]

{ #category : #accessing }
IceTipHistoryContext >> item: anObject [
	item := anObject
]

{ #category : #accessing }
IceTipHistoryContext >> selectedCommit [
	^ self item
]
