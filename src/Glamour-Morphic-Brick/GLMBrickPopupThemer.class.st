"
I'm a style module related to popup
"
Class {
	#name : #GLMBrickPopupThemer,
	#superclass : #GLMBrickThemerModule,
	#category : #'Glamour-Morphic-Brick-Widgets-UI-Popup-Themer'
}

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> backgroundColor [
	"Return a background color of a default popup"

	^ self theme backgroundColor "delegate to current Pharo theme"
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> borderColor [
	"Return a border color of a default popup"

	^ self theme borderColor "delegate to current Pharo theme"
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> borderWidth [
	"Return a border width of a default popup"

	^ self theme borderWidth "delegate to current Pharo theme"
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> contentBackgroundColor [
	"Return a background color of a content brick"

	^ Color transparent
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> contentPadding [
	"Return padding of popup's content brick"

	^ 0
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> inset [
	"Return by how much the whole popup's content (including trigger and content) should be inset.
	Basically, it works similar to padding"
	
	^ self borderWidth
]

{ #category : #'brick-popup-shadow' }
GLMBrickPopupThemer >> shadowColor [
	"Return a color of a shadow around a popup"
	
	^ Color black alpha: 0.5
]

{ #category : #'brick-popup-shadow' }
GLMBrickPopupThemer >> shadowInset [
	"Return an inset of a shadow around a popup"

	^ 0
]

{ #category : #'brick-popup-shadow' }
GLMBrickPopupThemer >> shadowOffset [
	"Return an offset of a shadow around popup"
	
	^ 0@0
]

{ #category : #'brick-popup-shadow' }
GLMBrickPopupThemer >> shadowWidth [
	"Return a width of a shadow around a popup"
	
	^ 0
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> spacing [
	"Return how much free space there should be between trigger and content bricks"

	^ 0
]

{ #category : #'brick-popup' }
GLMBrickPopupThemer >> textColor [

	^  self theme textColor 
]
