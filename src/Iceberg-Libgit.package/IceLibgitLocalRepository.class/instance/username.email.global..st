accessing
username: aName email: anEmail global: globalBoolean
	"set user.name and user.email properties. 
	 this could be a nonsense with other backends, but git has them and needs them :)"
	self withRepoDo: [ :repo | | config |
		config := repo config.
		globalBoolean ifTrue: [ config := config openGlobal ].
		config 
			setString: 'user.name' to: aName;
			setString: 'user.email' to: anEmail ]