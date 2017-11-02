Extension { #name : #SolidFillStyle }

{ #category : #'*Morphic-Base' }
SolidFillStyle >> addFillStyleMenuItems: aMenu hand: aHand from: aMorph [
	"Add the items for changing the current fill style of the receiver"
	aMenu add: 'change color...' target: self selector: #changeColorIn:event: argument: aMorph
]

{ #category : #'*Morphic-Base' }
SolidFillStyle >> changeColorIn: aMorph event: evt [
	"Note: This is just a workaround to make sure we don't use the old color inst var"
	aMorph changeColorTarget: aMorph selector: #color: originalColor: self asColor hand: evt hand
]
