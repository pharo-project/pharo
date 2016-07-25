accessing
performAction: event
	"Inform the model that this button has been pressed. Sent by the controller when this button 
	 is pressed. If the button's actionSelector takes any arguments, they are obtained 
	 dynamically by sending the argumentSelector to the argumentsProvider"

	enabled ifFalse: [^self].
	askBeforeChanging ifTrue: [model okToChange ifFalse: [^ self]].
	
	"Toggle pressed"
	self pressed: self isPressed not.
	
	self actionBlock ifNotNil: [ ^ self actionBlock cull: self isPressed cull: event ].
	actionSelector ifNil: [ ^ self ].
	
	arguments := argumentsProvider 
		ifNotNil: [ argumentsProvider perform: argumentsSelector ]
		ifNil: [ #() ].
	model 
		perform: actionSelector 
		withArguments: (arguments copyWithFirst: self isPressed)