accessing
selectedClass
	"Answer the class in which the currently selected context's method was  found."
	^ selectedClass
		ifNil: [ selectedClass := selectedContext receiver
				ifNil: [ selectedSelector := selectedContext method selector.
					   selectedContext method methodClass ]
				ifNotNil: [ selectedContext methodClass ]]