expanding-collapsing
expandAllSilently: aMorph
	aMorph isExpanded ifFalse: [aMorph expand].
	aMorph childrenDo: [:ch | self expandAllSilently: ch].
	

