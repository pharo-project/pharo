Class {
	#name : #GLMBrickPopupEditBorderThemer,
	#superclass : #GLMBrickPopupEditThemer,
	#category : #'Glamour-Morphic-Brick-Widgets-UI-Popup-Themer'
}

{ #category : #'brick-popup' }
GLMBrickPopupEditBorderThemer >> backgroundColor [
	^ self borderColor muchLighter darker
]

{ #category : #'brick-popup' }
GLMBrickPopupEditBorderThemer >> contentBackgroundColor [
	^ self theme backgroundColor "delegate to current Pharo theme"
]

{ #category : #'brick-popup' }
GLMBrickPopupEditBorderThemer >> contentPadding [
	^ 5
]
