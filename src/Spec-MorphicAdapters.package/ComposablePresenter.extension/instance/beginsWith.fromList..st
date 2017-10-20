*Spec-MorphicAdapters
beginsWith: aString fromList: aMorph
	"This method is used bu the list for the search of elements when you are typing directly in the list"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter beginsWith: aString fromList: aMorph ]