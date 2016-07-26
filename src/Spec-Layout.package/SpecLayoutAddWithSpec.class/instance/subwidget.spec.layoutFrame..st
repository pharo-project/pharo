instance creation
subwidget: sub spec: aSpecSelector layoutFrame: aFrameLayout

	self subwidget: (sub isArray 
						ifTrue: [ #(model), sub , {#retrieveSpec:. aSpecSelector} ]
						ifFalse: [ {#model. sub . #retrieveSpec: . aSpecSelector } ]).
						
	self layoutFrame: aFrameLayout.
	self generateArguments