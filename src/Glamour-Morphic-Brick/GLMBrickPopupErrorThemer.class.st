Class {
	#name : #GLMBrickPopupErrorThemer,
	#superclass : #GLMBrickPopupThemer,
	#category : 'Glamour-Morphic-Brick-Widgets-UI-Popup-Themer'
}

{ #category : #brick-popup }
GLMBrickPopupErrorThemer >> backgroundColor [
	"FBC9C1"
	^ Color r: 251 g: 201 b: 193 range: 255
]

{ #category : #brick-popup }
GLMBrickPopupErrorThemer >> borderColor [

	^ Color r: 225 g: 124 b: 108 range: 255
]

{ #category : #brick-popup }
GLMBrickPopupErrorThemer >> textColor [

	^ Color r: 206 g: 38 b: 11 range: 255
]
