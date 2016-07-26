accessing
transform: aTransform 
	"
	- transform is an Affine matrix, used to map path geometry to screen,
	and therefore used to calculate the error tolerance for path subdivisions
	
	
	- a tolerance multiplier is a number .. which
		if = 1 , the default error tolerance is used,
		if > 1 you will get more coarse approximation
		if < 1 you will get more finer approximation
	
	"
	
	transform := aTransform.
	
