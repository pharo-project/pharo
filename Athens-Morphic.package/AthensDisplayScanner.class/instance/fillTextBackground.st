displaying
fillTextBackground
	backgroundColor ifNotNil: [ canvas drawShape: (line left @ lineY extent: line width @ line lineHeight) ]