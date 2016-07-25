initialization
initialize

	super initialize.

	helpHolder := nil asValueHolder.
	borderWidth := 0 asValueHolder.
	borderColor := Color transparent asValueHolder.
	enabledHolder := true asValueHolder.
	dragEnabled := false asValueHolder.
	dropEnabled := false asValueHolder.
	dragTransformationBlock := [ :item :source | item ] asValueHolder.
	wantDropBlock := [ :draggedItem :event :source | self dropEnabled ] asValueHolder.
	acceptDropBlock := [ :draggedItem :event :source | ] asValueHolder.
	transferBlock := [:passenger :source | self transferFor: passenger from: source ] asValueHolder.
	color := self defaultColor asValueHolder.
	
	helpHolder whenChangedDo: [:string | 
		self widget ifNotNil: [:w | w update: #setBalloonText: with: { string } ] ].
	
	borderWidth whenChangedDo: [:int | 
		self widget ifNotNil: [:w | w update: #borderWidth: with: { int } ] ].
	
	borderColor whenChangedDo: [:newColor | 
		self widget ifNotNil: [:w | w update: #borderColor: with: { newColor } ] ].
	
	color whenChangedDo: [:newColor | 
		self widget ifNotNil: [:w | w update: #color: with: { newColor } ] ].
	
	enabledHolder whenChangedDo: [:b |
		self widget ifNotNil: [:w | w update: #enabled: with: { b } ] ].
	
	dragEnabled whenChangedDo: [:b |
		self widget ifNotNil: [:w | w update: #dragEnabled: with: { b } ] ].
	
	dropEnabled whenChangedDo: [:b |
		self widget ifNotNil: [:w | w update: #dropEnabled: with: { b } ] ].