class initialization
initialize
	IdCounter := 0.
	self environment 
		at: #SessionManager 
		ifPresent: [ :manager | manager default registerNetworkClassNamed: self name ]
		ifAbsent: [ Smalltalk addToStartUpList: self ]