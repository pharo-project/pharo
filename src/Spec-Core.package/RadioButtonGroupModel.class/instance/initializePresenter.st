initialization
initializePresenter
	self
		whenCanDeselectByClickChanged: [ :aBoolean | buttons do: [ :each | each canDeselectByClick: aBoolean ] ]