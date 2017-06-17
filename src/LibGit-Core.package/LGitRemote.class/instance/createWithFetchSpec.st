private
createWithFetchSpec
	self withReturnHandlerDo: [ 
		self
			remote_create_with_fetchspec: self
			repo: self repository
			name: self remoteName
			url: self url
			fetch: self fetchSpec ]