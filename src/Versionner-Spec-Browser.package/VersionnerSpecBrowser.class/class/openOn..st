ui
openOn: configurations
	| browser |
	browser := self new. 
	browser configurationList: configurations.
	browser openWithSpec.
	^ browser.