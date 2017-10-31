tutorial metainfo
tutorials
	^ (self subclasses sort: [ :a :b | a name < b name ])
		reject: [ :aTutorial | aTutorial package name = 'ProfStef-Tests' ]