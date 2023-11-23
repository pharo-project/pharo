"
I model temp read
"
Class {
	#name : 'RFTempRead',
	#superclass : 'RFVariableRead',
	#instVars : [
		'context'
	],
	#category : 'Reflectivity-Operations',
	#package : 'Reflectivity',
	#tag : 'Operations'
}

{ #category : 'accessing' }
RFTempRead >> context [
	^ context
]

{ #category : 'accessing' }
RFTempRead >> context: anObject [
	context := anObject
]

{ #category : 'evaluating' }
RFTempRead >> value [
	^variable readInContext: context
]

{ #category : 'accessing' }
RFTempRead >> variableName: aSymbol [
	variable := context lookupVar: aSymbol
]
