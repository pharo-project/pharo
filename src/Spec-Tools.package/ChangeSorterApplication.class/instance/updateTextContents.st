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
		ifTrue: [ textModel aboutToStyle: false ]
		ifFalse: [ textModel aboutToStyle: true ] .
	textModel text: text.