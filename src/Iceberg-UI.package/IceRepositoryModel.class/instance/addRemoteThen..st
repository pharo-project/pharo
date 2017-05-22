actions remotes
addRemoteThen: aBlock
	IceRemoteModel new 
		setModal: true;
		repository: self repository;
		onAccept: [ :remote |
			self repository addRemote: remote. 
			aBlock cull: remote ];
		openWithSpec