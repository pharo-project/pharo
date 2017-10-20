utilities
streamOn: collection upToAll: subcollection1 upToAll: subcollection2 
	^ collection readStream
		upToAll: subcollection1;
		upToAll: subcollection2