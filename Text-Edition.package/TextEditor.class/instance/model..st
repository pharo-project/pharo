accessing
model: aModel 
	"Controller|model: and Controller|view: are sent by View|controller: in 
	order to coordinate the links between the model, view, and controller. In 
	ordinary usage, the receiver is created and passed as the parameter to 
	View|controller: so that the receiver's model and view links can be set 
	up by the view."

	model := aModel