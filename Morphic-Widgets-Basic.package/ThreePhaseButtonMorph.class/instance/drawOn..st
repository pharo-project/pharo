drawing
drawOn: aCanvas

	state == #off ifTrue: [
		offImage ifNotNil: [aCanvas translucentImage: offImage at: bounds origin]].
	state == #pressed ifTrue: [
		pressedImage ifNotNil: [aCanvas translucentImage: pressedImage at: bounds origin]].
	state == #on ifTrue: [
		image ifNotNil: [aCanvas translucentImage: image at: bounds origin]].