selection change
updateLastClickedMorph
	"When the contents have been updated, ensure that lastClickedMorph refer to a correct morph."

	lastClickedMorph
		ifNil: [ lastClickedMorph := self selectedMorphList ifEmpty: [  ] ifNotEmpty: [ self selectedMorphList last ] ]
		ifNotNil: [ 
			self allNodeMorphs
				do: [ :each | 
					each complexContents withoutListWrapper = lastClickedMorph complexContents withoutListWrapper
						ifTrue: [ lastClickedMorph := each ] ] ]