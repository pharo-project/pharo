comparing
hash

	self class == ReadWriteStream ifFalse: [^ super hash].
	^ (self position + readLimit + 53) hash