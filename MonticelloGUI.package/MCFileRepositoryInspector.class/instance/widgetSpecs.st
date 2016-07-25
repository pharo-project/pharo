morphic ui
widgetSpecs
	| searchBarOffset |
	
	searchBarOffset := 30 + StandardFonts defaultFont height + 10..
	
	^{ {{#buttonRow}.            {0. 0. 1. 0}.     {0. 0. 0. 30}}.
		
		{{#packageSearchField}.   {0. 0. 0.5. 0}.   {0. 30. 0. searchBarOffset}}.
		{{#listMorph:selection:menu:icon: . #packageList . #packageSelection . #packageListMenu: . #packageIcon:}.{0. 0. 0.5. 0.6}. {0. searchBarOffset+3. 0. 0}}.
		
		{{#versionSearchField}.   {0.5. 0. 1. 0}.   {0. 30. 0. searchBarOffset}}.
		{{#listMorph:. #version}. {0.5. 0. 1. 0.6}. {0. searchBarOffset+3. 0. 0.}}.
		
		{{#textMorph:. #summary}. {0. 0.6. 1. 1}.   {0. 0. 0. 0.}}}