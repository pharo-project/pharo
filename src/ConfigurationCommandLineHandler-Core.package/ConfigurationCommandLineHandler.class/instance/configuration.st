accessing
configuration
	| gofer |
	gofer := self gofer.
	self hasConfiguration 
		ifTrue: [ gofer package: self configurationName ]
		ifFalse: [ gofer configuration ].
	gofer load.
	^ Smalltalk globals at: self configurationName asSymbol