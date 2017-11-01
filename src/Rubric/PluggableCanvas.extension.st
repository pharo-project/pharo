Extension { #name : #PluggableCanvas }

{ #category : #'*Rubric-Editing-Core' }
PluggableCanvas >> rubParagraph: paragraph bounds: bounds color: color [
	self apply: [ :c |
		c paragraph: paragraph bounds: bounds color: color ]
]
