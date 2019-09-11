"
A GLMBrickThemer is the main Brick themer class where styles for core and basic Bricks are defined.

Instance Variables
	modules:		<Dictionary>

modules
	- dictionary with all registered style modules with association module name -> module

"
Class {
	#name : #GLMBrickThemer,
	#superclass : #Object,
	#traits : 'GLMBrickScrollpaneStyle + GLMBrickRubricStyle + GLMBrickWindowStyle + GLMBrickButtonStyle + GLMBrickCheckBoxStyle + GLMBrickTextStyle',
	#classTraits : 'GLMBrickScrollpaneStyle classTrait + GLMBrickRubricStyle classTrait + GLMBrickWindowStyle classTrait + GLMBrickButtonStyle classTrait + GLMBrickCheckBoxStyle classTrait + GLMBrickTextStyle classTrait',
	#instVars : [
		'modules'
	],
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'theme-actions' }
GLMBrickThemer >> actionThemer [

	^ GLMActionBrickThemer new
]

{ #category : #'brick-modules' }
GLMBrickThemer >> addModule: aThemerModule named: aSymbol [

	self modules at: aSymbol put: aThemerModule.
	aThemerModule themer: self.
	^ aThemerModule
]

{ #category : #brick }
GLMBrickThemer >> backgroundColor [

	^ Color transparent
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> color [

	^ self
		registerModule: GLMBrickColorThemer new
]

{ #category : #brick }
GLMBrickThemer >> contentBackgroundColor [

	^ Color white
]

{ #category : #'brick-modules' }
GLMBrickThemer >> doesNotUnderstand: aMessage [

	^ self modules at: aMessage selector ifAbsent: [ super doesNotUnderstand: aMessage ]
	
]

{ #category : #brick }
GLMBrickThemer >> focusedBorderColor [

	^ self color peterRiver
]

{ #category : #brick }
GLMBrickThemer >> focusedBorderWidth [

	^ 1
]

{ #category : #'brick-modules' }
GLMBrickThemer >> modules [

	^ modules ifNil: [ modules := Dictionary new ]
]

{ #category : #brick }
GLMBrickThemer >> normalBorderColor [

	^ self color silver
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupEditorBorderThemer [

	^ self
		registerModule: GLMBrickPopupEditBorderThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupEditorShadowThemer [

	^ self
		registerModule: [ GLMBrickPopupEditShadowThemer new ]
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupErrorThemer [

	^ self
		registerModule: GLMBrickPopupErrorThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupHelpThemer [

	^ self
		registerModule: GLMBrickPopupHelpThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupSuccessThemer [

	^ self
		registerModule: GLMBrickPopupSuccessThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupThemer [

	^ self
		registerModule: GLMBrickPopupThemer new
]

{ #category : #'brick-modules' }
GLMBrickThemer >> registerModule: aModule [

	^ self
		registerModule: aModule
		to: self
		named: thisContext sender selector
]

{ #category : #'brick-modules' }
GLMBrickThemer >> registerModule: aModule to: aThemer [

	^ self
		registerModule: aModule
		to: aThemer
		named: thisContext sender selector
]

{ #category : #'brick-modules' }
GLMBrickThemer >> registerModule: aModule to: aThemer named: aString [
	"Registers aModule to aThemer. It stores modules in a dictionaty where a key is a selector of my caller method. It allows to customize bricks that are not added to the application (in this case they don't have access to custom themer normaly, because owner is nil) using the same api as added one."
	^ self modules
		at: aString
		ifAbsent: [ aThemer addModule: aModule value named: aString ]
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> tabLabelThemer [

	^ self
		registerModule: GLMBrickTabLabelThemer new
]

{ #category : #brick }
GLMBrickThemer >> textColor [

	^ self color darken
]

{ #category : #brick }
GLMBrickThemer >> textLightColor [

	^ Color gray
]

{ #category : #brick }
GLMBrickThemer >> textSecondaryColor [

	^ Color r:25 g: 35 b: 45 range: 255
]

{ #category : #accessing }
GLMBrickThemer >> theme [
	^ Smalltalk ui theme
]
