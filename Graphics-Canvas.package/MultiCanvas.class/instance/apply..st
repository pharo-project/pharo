private
apply: aCommand

	self flag: #roundedRudeness.	
	"This rudeness is to help get rounded corners to work right on RemoteCanvases. Since the RemoteCanvas has no other way to read its bits, we are grabbing them from Display for now. To support this, we need to see that the Display is written before any RemoteCanvases"

	canvases do: [ :canvas | 
		(canvas isKindOf: FormCanvas) ifTrue: [aCommand value: canvas]
	].
	canvases do: [ :canvas | 
		(canvas isKindOf: FormCanvas) ifFalse: [aCommand value: canvas]
	].
