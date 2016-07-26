package filter
buildPackageSearch
	"It is an search dialog which filters package list"
	| textMorph |
	
	searchWidget := SearchMorph new
		model: self;
		updateSelector: #packageSearchUpdate:;
		ghostText: 'Type: Pkg1|^Pkg2|Pk.*Core$';
		list: SharedPackageFilterPatternsHolder instance;
		useSelectionIndex: false;
		setIndexSelector: #packageSearchUpdate:;
		yourself.
		
	textMorph := searchWidget contentMorph textMorph.	
	self setShortcuts: #NautilusGlobalShortcuts to: textMorph.
	textMorph bindKeyCombination: Character tab asKeyCombination toAction: [ textMorph navigateFocusForward ].
	
	
	^ searchWidget