initialize-release
newFaceFromFile: fileName index: anInteger
	self primNewFaceFromFile: fileName fullName index: anInteger.
	self class register: self.