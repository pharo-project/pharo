installing
installVersion: aStringOrSymbol 
	[ Gofer it 
			url: self repositoryUrl;
			configurationOf: self name;
			loadVersion: aStringOrSymbol ]
	on: Warning do: [ :w | w resume: true ]