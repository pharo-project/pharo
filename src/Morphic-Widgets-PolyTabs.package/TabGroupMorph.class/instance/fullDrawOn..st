drawing
fullDrawOn: aCanvas
	"Patch up the selected tab visuals if required."
	
	super fullDrawOn: aCanvas.
	self theme drawTabGroupFinishingFor: self on: aCanvas