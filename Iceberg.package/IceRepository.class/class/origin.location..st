instance creation
origin: url location: location
	^ self new 
		location: location;
		origin: (IceRemoteRepository url: url);
		yourself
