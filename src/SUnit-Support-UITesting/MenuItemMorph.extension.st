Extension { #name : #MenuItemMorph }

{ #category : #'*SUnit-Support-UITesting' }
MenuItemMorph >> itemWithWording: wording [
	"If any of the receiver's items or submenu items have the given wording (case-blind comparison done), then return it, else return nil."
	(self contents asString sameAs: wording) ifTrue:[^self].
	subMenu ifNotNil:[^subMenu itemWithWording: wording].
	^nil
]
