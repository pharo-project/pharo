Extension { #name : 'HandMorph' }

{ #category : '*Morphic-Widgets' }
HandMorph >> mouseClickStateFor: aMorph event: aMouseEvent [
	mouseClickState := MouseClickState client: aMorph event: aMouseEvent.
	^ mouseClickState
]
