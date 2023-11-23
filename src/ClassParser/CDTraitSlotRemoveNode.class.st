"
MyTrait -- {#slotName}
"
Class {
	#name : 'CDTraitSlotRemoveNode',
	#superclass : 'CDTraitCompositionNode',
	#instVars : [
		'removedSlots'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'accessing' }
CDTraitSlotRemoveNode >> removedSlots [

	^ removedSlots
]

{ #category : 'accessing' }
CDTraitSlotRemoveNode >> removedSlots: anObject [

	removedSlots := anObject
]

{ #category : 'printing' }
CDTraitSlotRemoveNode >> toString [
	^ subject name ,' -- ', removedSlots asString
]
