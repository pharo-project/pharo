instance creation
generateArguments
	| resizingMode resizing width height dimensions |
	width := 0.
	height := 0.
	dimensions := OrderedCollection new.
	
	(size > 0) 
		ifTrue: [ "fixed size"
			(orientation == #horizontal) 
				ifTrue: [ width := size ] 
				ifFalse: [ height := size ].
			resizingMode := #rigid. ]
		ifFalse: [ "flexible"
			dimensions add: #spaceFillWeight:; add: spaceFillWeight.
			resizingMode := #spaceFill. ].
	
	dimensions add: #width:; add: width; add: #height:; add: height.
	
	resizing := 
		(orientation == #horizontal) 
			ifTrue: [ {#hResizing: . resizingMode . #vResizing: . #shrinkWrap} ]
			ifFalse: [ {#hResizing: . #shrinkWrap . #vResizing: . resizingMode} ].
	
	self arguments: { {subwidget} , resizing , dimensions }