model access
setValue: newValue
	"Bypass screwed up scrollbar!"
	
	^self perform: #setValue: withArguments: {newValue} inSuperclass: Slider