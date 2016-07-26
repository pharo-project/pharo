comparing
hash

	^ (self class hash
		bitXor: self versionName hash)
		bitXor: self repositoryDescription hash