converting
asArray
	commands 
		select: [ :c | c isKindOf: SpecTableLayoutAddSpacer ] 
		thenDo: [ :c | c orientation: self orientation ].
	
	^ { #TableContainerPresenter . #layoutProperties: . layoutProperties } , 
	  (commands flatCollect: #asSpecElements)