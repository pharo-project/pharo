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
		act: [ :text :m | self updateOuterPaneFrom: text for: method ]
			icon: GLMUIThemeExtraIcons glamorousAccept
			on: $s
			entitled: 'Accept';
		act: [ :text :m | 
			Smalltalk tools browser 
				openOnClass: m methodClass 
				selector: m selector ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			entitled: 'Browse';
		act: [ :text :m | 
			(text pane port: #evaluatedObject) value: m ]
			icon: GLMUIThemeExtraIcons glamorousInspect
			entitled: 'Inspect'