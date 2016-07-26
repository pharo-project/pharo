private
proxy: anExampleProxy
	| examples |
	examples := OrderedCollection new.
	self class: anExampleProxy provider class selector: anExampleProxy selector to: examples.
	^ examples first