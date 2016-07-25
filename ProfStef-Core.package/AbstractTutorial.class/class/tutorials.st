tutorial metainfo
tutorials
	^ (self subclasses 
		sort: [:a :b |  a name < b name]) 
		select: [:aTutorial |
			(aTutorial category = 'ProfStef-Tests') not ].
