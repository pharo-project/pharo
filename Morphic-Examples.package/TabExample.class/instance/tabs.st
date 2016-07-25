private
tabs
	
	^ OrderedCollection new
		add: self storedListTab;
		add: self freshListTab;
		add: self blueMorphTab;
		add: self redMorphTab;
		yourself