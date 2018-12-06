Extension { #name : #Form }

{ #category : #'*Polymorph-Widgets-Core' }
Form >> listRenderOn: aCanvas atRow: index bounds: aRectangle color: aColor backgroundColor: backgroundColor from: aList [
	self asMorph
		listRenderOn: aCanvas
		atRow: index
		bounds: aRectangle
		color: aColor
		backgroundColor: backgroundColor
		from: aList
]

{ #category : #'*Polymorph-Widgets-Core-Themes' }
Form >> mergeBottomRightWith: aForm [
	^ self 
		mergeWith: aForm 
		at: self extent - aForm extent
]

{ #category : #'*Polymorph-Widgets-Core-Themes' }
Form >> mergeWith: aForm [
	^ self mergeWith: aForm at: 0@0
]

{ #category : #'*Polymorph-Widgets-Core-Themes' }
Form >> mergeWith: aForm at: aPoint [
	| mergedForm |

	mergedForm := self deepCopy.
	mergedForm getCanvas 
		translucentImage: aForm 
		at: aPoint. 
		
	^ mergedForm 
]

{ #category : #'*Polymorph-Widgets-Core-Themes' }
Form >> setAsBackground [
	"Set this form as a background image."
	| world |
	world := self currentWorld.
	world backgroundMorph: ((Smalltalk ui theme builder newAlphaImage: self help: nil) autoSize: false;
			 layout: #scaled;
			 lock) 
]
