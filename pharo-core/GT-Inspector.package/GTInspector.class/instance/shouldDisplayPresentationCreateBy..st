context
shouldDisplayPresentationCreateBy: aMethod
	"Verify whether or not the presentation created by the given method should be displayed.
	If no filter is given display all presentations."
	^ self presentationFilter 
		ifNil: [ true]
		ifNotNil: [ self presentationFilter shouldDisplayPresentationCreateBy: aMethod ]