Extension { #name : #RGInstanceVariableSlot }

{ #category : #'*Ring-RuntimeSupport' }
RGInstanceVariableSlot >> emitStore: methodBuilder [

	methodBuilder storeInstVar: self index
]

{ #category : #'*Ring-RuntimeSupport' }
RGInstanceVariableSlot >> emitValue: methodBuilder [

	methodBuilder pushInstVar: self index.

]

{ #category : #'*Ring-RuntimeSupport' }
RGInstanceVariableSlot >> index [

	"TODO!"
	^ self parent parent allSlots indexOf: self
]
