*Polymorph-Widgets
rememberKeyboardFocus: aMorph
	"Record the current keyboard focus for the receiver."
	
	(aMorph isNil or: [(aMorph hasOwner: self) not]) ifFalse: [
		self setProperty: #rememberedFocus toValue: aMorph]