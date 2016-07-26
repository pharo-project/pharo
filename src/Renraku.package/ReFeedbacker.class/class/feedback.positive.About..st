actions
feedback: aComment positive: aBoolean About: aReCritic 
	[ 
	ZnClient new
		beOneShot;
		http;
		host: 'renraku.inf.usi.ch';
		addPath: 'feedback';
		formAt: 'uuid' put: self uuid asString;
		formAt: 'pharoVersion' put: SystemVersion current highestUpdate asString;
		formAt: 'author' put: Author fullName;
		formAt: 'rule' put: aReCritic rule class name;
		formAt: 'target' put: aReCritic sourceAnchor entity name;
		formAt: 'time' put: DateAndTime now asString;
		formAt: 'positive' put: aBoolean asString;
		formAt: 'comment' put: aComment;
		post ]
		on: NetworkError
		do: [ :ex | ^ false ].
		
	^ true