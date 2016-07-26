copying
trimRight: aBlock
	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock