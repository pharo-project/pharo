actions
actions
	^ super actions copyWith: 
		(RePropertyAction new
			icon: #smallWindow asIcon;
			description: 'Open dependency analyzer';
			action: [ :crit :package |
				self openDependencyAnalyzerOn: package ];
			yourself)
				