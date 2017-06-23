accessing
defaultPrivateKey 	
	self flag: #todo. "In fact this is different for windows. We need to change this."
	^ (FileLocator home asFileReference / '.ssh' / 'id_rsa') pathString