private
updateTextContents

	| text |
	text :=  (self model 
				setContentsOfChangeSet: self selectedChangeSet
				forClass: self selectedClass
				andSelector: self selectedSelector
				prettyPrint: prettyPrint
				showDiff: showDiff).
	({'Method was added, but cannot be found!'. 'Added then removed (see versions)'. 'Method has been removed (see versions)'} includes: text)
		ifTrue: [ textPresenter aboutToStyle: false ]
		ifFalse: [ textPresenter aboutToStyle: true ] .
	textPresenter text: text.