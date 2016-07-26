page
tabOfPage: aMorph
	"Answer the tab associated with the given page."

	^self tabSelectorMorph tabs at: (self pages indexOf: aMorph)