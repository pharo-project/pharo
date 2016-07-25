copying
trimLeft: aBlock
	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."
	
	^ self trimLeft: aBlock right: [ :char | false ]