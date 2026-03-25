Extension { #name : 'ClassDescription' }

{ #category : '*Shift-ClassBuilder' }
ClassDescription >> addSlot: aSlot [

	<reflection: 'Class structural modification - Slot modification'>
	^self subclassResponsibility
]

{ #category : '*Shift-ClassBuilder' }
ClassDescription >> removeSlot: aSlot [
	<reflection: 'Class structural modification - Slot modification'>
	^self subclassResponsibility
]
