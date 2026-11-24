Extension { #name : 'HandMorph' }

{ #category : '*Morphic-Base' }
HandMorph >> mouseClickStateFor: aMorph event: aMouseEvent [
	mouseClickState := MouseClickState client: aMorph event: aMouseEvent.
	^ mouseClickState
]
