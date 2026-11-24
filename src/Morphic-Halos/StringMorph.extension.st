Extension { #name : 'StringMorph' }

{ #category : '*Morphic-Halos' }
StringMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' selector: #changeFont.
	aCustomMenu add: 'change emphasis' selector: #changeEmphasis
]

{ #category : '*Morphic-Halos' }
StringMorph >> changeEmphasis [

	| reply |
	reply := self morphicUIManager
				chooseFrom: (self emphasisChoices collect: [:t | t translated])
				values: self emphasisChoices.
	reply ifNotNil: [ self emphasis: (TextEmphasis perform: reply) emphasisCode ]
]

{ #category : '*Morphic-Halos' }
StringMorph >> emphasisChoices [
	"Returns the emphasis selectors that are sent to a TextEmphasis."

	^ #(normal bold italic narrow underlined struckOut)
]
