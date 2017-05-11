building
sourceOf: method for: object in: composite
	^ composite pharoMethod 
		display: [ :m | m sourceCode ];
		smalltalkClass: [ :m | m methodClass ];
		doItReceiver: [ object ];
		selectionPopulate: #evaluatedObject
			on: $g
			entitled: 'Do it and go'
			with: [ :text | Smalltalk compiler evaluate: text selectedText for: object logged: false ];
		installDefaultSelectionActions;
		selectionAct: [ :text | self updateOuterPaneFrom: text for: method ]
			icon: GLMUIThemeExtraIcons glamorousAccept
			on: $s
			entitled: 'Accept'