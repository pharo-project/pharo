api
model: aComposableModel
	aComposableModel owner: self.
	self retrievingBlock: [ aComposableModel buildWithSpec asWidget ]