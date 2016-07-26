class initialization
initialize
	ManagedServers := IdentitySet new.
	AlwaysRestart := true.
	self environment 
		at: #SessionManager 
		ifPresent: [ :manager | manager default registerNetworkClassNamed: self name ]
		ifAbsent: [ Smalltalk addToStartUpList: self; addToShutDownList: self ]
	