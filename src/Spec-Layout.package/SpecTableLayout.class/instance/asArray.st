converting
asArray
	commands 
		select: [ :c | c isKindOf: SpecTableLayoutAddSpacer ] 
		thenDo: [ :c | c orientation: self orientation ].
	
	^ { #TableContainerModel . #layoutProperties: . layoutProperties } , 
	  (commands flatCollect: #asSpecElements)