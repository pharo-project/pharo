api
retrieveSpec: aSelector
	| layout |
	
	layout := self class perform: aSelector.
	layout isSpecLayout ifTrue: [ layout selector: aSelector ].
	^ layout