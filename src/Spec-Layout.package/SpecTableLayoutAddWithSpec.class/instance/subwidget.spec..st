instance creation
subwidget: sub spec: aSpecSelector
	self subwidget: (sub isArray 
						ifTrue: [ #(model), sub , {#retrieveSpec:. aSpecSelector} ]
						ifFalse: [ {#model. sub . #retrieveSpec: . aSpecSelector } ]).
	
	self generateArguments