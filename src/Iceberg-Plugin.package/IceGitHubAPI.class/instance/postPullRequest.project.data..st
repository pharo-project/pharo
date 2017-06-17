requesting
postPullRequest: owner project: projectName data: aDictionary
	^ self 
		post: 'repos/', owner, '/', projectName, '/pulls'
		with: [ :client |
			client 
				entity: (ZnEntity text: (STON toJsonString: aDictionary));
				contentType: ZnMimeType applicationJson ]