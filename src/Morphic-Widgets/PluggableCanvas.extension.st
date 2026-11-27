Extension { #name : 'PluggableCanvas' }

{ #category : '*Morphic-Widgets' }
PluggableCanvas >> rubParagraph: paragraph bounds: bounds color: color [
	self apply: [ :c |
		c rubParagraph: paragraph bounds: bounds color: color ]
]
