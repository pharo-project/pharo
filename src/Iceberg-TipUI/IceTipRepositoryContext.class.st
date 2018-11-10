Class {
	#name : #IceTipRepositoryContext,
	#superclass : #IceTipContext,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepositoryContext >> item [
	^ item
]

{ #category : #accessing }
IceTipRepositoryContext >> item: anIceTipCachedModel [ 
	
	item := anIceTipCachedModel
]
