menu spec
widgetSpecs
	| searchBarOffset |
	
	searchBarOffset := 30 + StandardFonts defaultFont height + 10.
	
	^ {
		"send fractions offsets"
		{{#buttonRow}. {0. 0. 1. 0}. {0. 0. 0. 30.}}.
		
		{{#workingCopySearchField}. {0. 0. 0.5. 0.}. {0. 30. 0. searchBarOffset.}}.
		{{#treeMorph:. #workingCopy}. {0. 0. 0.5. 1}. {0. searchBarOffset+3. 0. 0.}}.
		
		{{#repositorySearchField}. {0.5. 0. 0.85. 0.}. {0. 30. 0. searchBarOffset.}}.
		{{#showOnlyRepositoriesFromWorkingCopyField}. { 0.85. 0. 1. 0.}. { 0. 30. 0. searchBarOffset .}}.
		{{#listMorph:selection:menu:icon: . #repositoryList . #repositorySelection . #repositoryListMenu: . #repositoryIcon:}. {0.5. 0. 1. 1}. {0. searchBarOffset+3. 0. 0.}}.
		}.