Extension { #name : 'MenuItemMorph' }

{ #category : '*Morphic-Testing-Support' }
MenuItemMorph >> itemWithWording: wording [
	"If any of the receiver's items or submenu items have the given wording (case-blind comparison done), then return it, else return nil."

	(self contents asString sameAs: wording) ifTrue: [ ^ self ].
	self subMenu ifNotNil: [ ^ self subMenu itemWithWording: wording ].
	^ nil
]
