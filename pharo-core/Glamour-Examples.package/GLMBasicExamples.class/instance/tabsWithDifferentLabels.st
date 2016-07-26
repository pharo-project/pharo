tabs examples
tabsWithDifferentLabels
	<glmBrowser: 'Tabs with different labels' input: '1'>
	"self new tabsWithDifferentLabels openOn: 1"
	
	| finder |
	finder := GLMFinder new.
	(1 to: 3) do: [:i | 
		finder show: [:a | 
			a text
				title: i printString;
				display: [:x | i printString].
			a text
				titleIcon: GLMUIThemeExtraIcons glamorousBrowse;
				display: [:x | i printString] ]
	].
	
	^ finder