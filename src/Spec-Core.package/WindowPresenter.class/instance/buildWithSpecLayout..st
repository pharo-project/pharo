private
buildWithSpecLayout: aSpec
	"Build the widget using the spec name provided as argument"
	| adapter widget |
	
	(self spec notNil and: [ self needRebuild not ])
		ifTrue: [ widget := self spec instance ]
		ifFalse: [
			adapter := self privateAdapterFromModel: self withSpec: self defaultSpecSelector.
			widget := adapter widget.
			self addModelIn: widget withSpecLayout: aSpec ].

	self ensureExtentFor: widget.
	
	^ widget