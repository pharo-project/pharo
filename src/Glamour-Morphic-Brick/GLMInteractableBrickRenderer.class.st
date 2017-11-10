"
A GLMInteractableBrickRenderer is more advanced Brick renderer, that is used in all interactable Bricks to allow on-fly change of background color or image

"
Class {
	#name : #GLMInteractableBrickRenderer,
	#superclass : #GLMBrickRenderer,
	#category : #Glamour-Morphic-Brick-Renderer
}

{ #category : #rendering }
GLMInteractableBrickRenderer >> render: aBrick on: aCanvas [

	self
		renderShadowOf: aBrick on: aCanvas;
		renderBackgroundColorOf: aBrick on: aCanvas;
		renderColorOf: aBrick on: aCanvas;
		renderImageOf: aBrick on: aCanvas;
		renderBorderOf: aBrick on: aCanvas
]

{ #category : #rendering }
GLMInteractableBrickRenderer >> renderColorOf: aBrick on: aCanvas [
	|color|
	color := aBrick currentColor.
	color ifNotNil: [
		aCanvas fillRectangle: (aBrick brickBounds expandBy: aBrick shadowWidth negated) color: color ]
]

{ #category : #rendering }
GLMInteractableBrickRenderer >> renderImageOf: aBrick on: aCanvas [
	|image|
	
	image := aBrick currentImage.
	
	image ifNotNil: [
		aCanvas
			translucentImage: (GLMBrick resizeImage: image scale: ((aBrick brickBounds expandBy: aBrick shadowWidth negated) extent / image extent))
			at: (aBrick brickBounds expandBy: aBrick shadowWidth negated) origin + (((aBrick paddingLeft - aBrick paddingRight)@(aBrick paddingTop - aBrick paddingBottom)) / 2.0) ]
]
