building
isPresentation: aPresentation inPane: aPane
	^ aPane presentations first cachedPresentation 
		ifNil: [ false ] 
		ifNotNil: [ :aCachedPresentation | aCachedPresentation first includes: aPresentation ]