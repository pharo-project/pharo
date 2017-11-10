"
A MenuLineMorph is a item separator in menu.

"
Class {
	#name : #MenuLineMorph,
	#superclass : #Morph,
	#category : #Morphic-Base-Menus
}

{ #category : #private }
MenuLineMorph >> baseColor [
	^ self theme autoMenuColor
		ifTrue: [ owner color twiceDarker ]
		ifFalse: [ 
			self theme currentSettings flatMenu
				ifFalse: [ owner color ]
				ifTrue: [ owner color twiceDarker ] ]
]

{ #category : #drawing }
MenuLineMorph >> drawOn: aCanvas [ 
	self theme flatMenu 
		ifFalse: [
			aCanvas
				fillRectangle: (bounds topLeft corner: bounds rightCenter)
				color: self baseColor twiceDarker .
			aCanvas
				fillRectangle: (bounds leftCenter corner: bounds bottomRight)
				color: self baseColor twiceLighter ]
		ifTrue: [
			aCanvas
				fillRectangle: (bounds topLeft corner: bounds bottomRight)
				color: self baseColor ]
]

{ #category : #drawing }
MenuLineMorph >> drawOnAthensCanvas: anAthensCanvas [
	self theme flatMenu
		ifFalse: [ 
			anAthensCanvas setPaint: self baseColor twiceDarker.
			anAthensCanvas drawShape: (bounds topLeft corner: bounds rightCenter).
			anAthensCanvas setPaint: self baseColor twiceLighter.
			anAthensCanvas drawShape: (bounds leftCenter corner: bounds bottomRight) ]
		ifTrue: [ 
			anAthensCanvas setPaint: self baseColor.
			anAthensCanvas drawShape: (bounds topLeft corner: bounds bottomRight) ]
]

{ #category : #initialization }
MenuLineMorph >> initialize [
	super initialize.
	self hResizing: #spaceFill; vResizing: #spaceFill.
]

{ #category : #testing }
MenuLineMorph >> isMenuLineMorph [
	^ true
]

{ #category : #layout }
MenuLineMorph >> minHeight [
	"Answer the receiver's minHeight"
	^ self theme menuBorderWidth
]

{ #category : #layout }
MenuLineMorph >> minWidth [
	"Answer the receiver's minWidth"
	^  10 
]
