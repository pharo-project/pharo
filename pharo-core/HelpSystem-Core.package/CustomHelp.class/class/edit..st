editing
edit: aSelector
	"Open a Workspace on the text in the given selector.
	When accepted, compile the result as a help topic."

	| topic |
	topic := (self respondsTo: aSelector) 
			ifTrue:[self perform: aSelector]
			ifFalse:[HelpTopic title: 'Untitled' contents: 'Please edit this topic. 
To change the topic title, edit the window label.'].
	UIManager default
		edit: topic contents
		label: topic title
		accept: [:text| 
			|newTitle|
			newTitle := UIManager default request: 'Save with title ' initialAnswer: topic title.		 
			newTitle ifNotNil: [ self accept: aSelector title: newTitle contents: text ]].
