user interaction
inform: msg withRef: fileRef
	self inform: (msg copyReplaceAll: '&fileRef' with: fileRef)