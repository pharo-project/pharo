Extension { #name : #SimpleServiceEntry }

{ #category : #'*Tool-FileList' }
SimpleServiceEntry >> addServiceFor: served toMenu: aMenu [
	aMenu 
		add: self label 
		target: self 
		selector: #performServiceFor: 
		argument: served.
	self usingLineAfter ifTrue: [ aMenu addLine ].
]
