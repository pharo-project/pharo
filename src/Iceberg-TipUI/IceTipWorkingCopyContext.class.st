Class {
	#name : #IceTipWorkingCopyContext,
	#superclass : #IceTipContext,
	#instVars : [
		'packageModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipWorkingCopyContext >> item [
	
	^ self packageModel
]

{ #category : #accessing }
IceTipWorkingCopyContext >> packageModel [
	^ packageModel
]

{ #category : #accessing }
IceTipWorkingCopyContext >> packageModel: anObject [
	packageModel := anObject
]
