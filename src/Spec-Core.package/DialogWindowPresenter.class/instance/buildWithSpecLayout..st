private
buildWithSpecLayout: aSpec
	"Build the widget using the spec name provided as argument"
	| widget |
	
	(self spec notNil and: [ self needRebuild not ])
		ifTrue: [ widget := self spec instance ]
		ifFalse: [ 
			contents := self model buildWithSpecLayout: aSpec.
			widget := self privateAdapterFromModel: self withSpec: self defaultSpecSelector.
			contents := nil ].

	self extent ifNotNil: [:ex | 
		(widget respondsTo: #extent:)
			ifTrue: [ widget extent: ex ]].
		
	^ widget