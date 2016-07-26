expanding-collapsing
expandSilently: aMorph suchThat: aBlock
	(aBlock value: aMorph complexContents)
		ifTrue: [
			aMorph isExpanded ifFalse: [aMorph expand].
			aMorph childrenDo: [:ch | self expandSilently: ch suchThat: aBlock]].
