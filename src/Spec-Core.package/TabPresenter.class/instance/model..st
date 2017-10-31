api
model: aComposablePresenter
	aComposablePresenter owner: self.
	self retrievingBlock: [ aComposablePresenter buildWithSpec asWidget ]