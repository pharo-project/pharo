Class {
	#name : #IceTipDiffContext,
	#superclass : #IceTipContext,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipDiffContext >> item [
	^ item
]

{ #category : #accessing }
IceTipDiffContext >> item: anObject [
	item := anObject
]
