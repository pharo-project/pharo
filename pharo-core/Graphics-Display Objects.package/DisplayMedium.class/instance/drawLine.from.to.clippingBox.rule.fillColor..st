displaying
drawLine: sourceForm from: beginPoint to: endPoint clippingBox: clipRect rule: anInteger fillColor: aForm 
	"Draw line by copying the argument, sourceForm, starting at location 
	beginPoint and ending at endPoint, clipped by the rectangle, clipRect. 
	The rule and mask for copying are the arguments anInteger and aForm."

	self subclassResponsibility