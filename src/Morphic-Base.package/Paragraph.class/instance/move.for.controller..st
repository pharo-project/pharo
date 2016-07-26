editing
move: anEvent for: model controller: editor
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	
	| action clickPoint |
	
	clickPoint := anEvent cursorPoint.
	action := false.
	
	self actionAttributesUnder: clickPoint event: anEvent do: [ :attribute| |target|
		"evaluate the attribute action"
		target := (model ifNil: [editor morph]).
		(attribute actOnMove: anEvent for: target in: self editor: editor) == true 	
			ifTrue: [ ^ true ]].
	
	(action and: [ Cursor currentCursor == Cursor webLink]) 
		ifTrue:[ Cursor normal show ].
	
	^ action