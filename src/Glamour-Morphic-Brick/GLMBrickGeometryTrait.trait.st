"
I declare api and logic to work with geometry of a Brick
"
Trait {
	#name : #GLMBrickGeometryTrait,
	#traits : 'GLMBrickExtensionTrait',
	#classTraits : 'GLMBrickExtensionTrait classTrait',
	#category : 'Glamour-Morphic-Brick-Traits'
}

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> bounds: aRectangle [
	"when you use this method, it changes brickBounds
	then recalculates layout and invalidates rectangle"
	self layoutAfter: [ :b | b wrappedBounds bounds: aRectangle ]
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> brickBounds [

	^ self wrappedBounds brickBounds
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> brickBounds: aRectangle [
	"directly sets brick bounds without layouting process"
	self wrappedBounds brickBounds: aRectangle
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> customHeight [

	^ self layout height brickValue: self
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> customWidth [

	^ self layout width brickValue: self
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> extent [

	^ self brickBounds extent
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> extent: aPoint [
	"actually works only if width or height are not custom"
	"updates layout"
	self changed.
	
	self width: aPoint x.
	self height: aPoint y.
	self wrappedBounds bounds: (self brickBounds origin extent: aPoint).
	
	self flag: 'do I need to layout morphs inside?'.
	self doLayout.
	
	self changed
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> globalBounds [
	"converts local bounds to global bounds on fly"
	
	^ self owner ifNotNil: [ self brickBounds translateBy: self owner globalBoundsTranslateFactor ] ifNil: [ self brickBounds ].
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> globalBoundsTranslateFactor [

	^ self owner ifNotNil: [ self owner globalBoundsTranslateFactor + self brickBounds origin ] ifNil: [ 0@0 ]
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> globalInnerBounds [
	"converts local inner bounds to global inner bounds on fly"
	
	^ self owner ifNotNil: [ self innerBounds translateBy: self owner globalBoundsTranslateFactor ] ifNil: [ self innerBounds ].
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> globalOuterBounds [
	"converts local outer bounds to global inner bounds on fly"
	
	^ self owner ifNotNil: [ self outerBounds translateBy: self owner globalBoundsTranslateFactor ] ifNil: [ self outerBounds ].
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> hMargin [

	^ self marginLeft + self marginRight
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> hPadding [

	^ self paddingLeft + self paddingRight
]

{ #category : #private-extension }
GLMBrickGeometryTrait >> hasProperty: aSymbol [

	^ self explicitRequirement
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> height [

	^ self brickBounds height
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> height: anObject [
	<brickBuilderProperty: #Geometry name: 'Height' as: #asInteger getter: #height model: #GLMUIBuilderTextFieldProperty>

	self privateHeight: anObject.
	self updateHeight
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> innerBounds [
	"can be used in layouts"
	^ self wrappedBounds innerBounds
]

{ #category : #brick-layout }
GLMBrickGeometryTrait >> layout [

	^ self explicitRequirement
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> margin [

	^ self wrappedBounds margin
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> margin: aCollection [
	<brickBuilderProperty: #Geometry name: 'Margin' as: #brickBuilderEvaluate getter: #margin model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds margin: aCollection
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginBottom [

	^ self wrappedBounds marginBottom
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginBottom: anObject [
	<brickBuilderProperty: #Geometry name: 'Margin Bottom' as: #asInteger getter: #marginBottom model: #GLMUIBuilderTextFieldProperty>
	
	self wrappedBounds marginBottom: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginLeft [

	^ self wrappedBounds marginLeft
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginLeft: anObject [
	<brickBuilderProperty: #Geometry name: 'Margin Left' as: #asInteger getter: #marginLeft model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds marginLeft: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginRight [

	^ self wrappedBounds marginRight
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginRight: anObject [
	<brickBuilderProperty: #Geometry name: 'Margin Right' as: #asInteger getter: #marginRight model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds marginRight: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginTop [

	^ self wrappedBounds marginTop
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> marginTop: anObject [
	<brickBuilderProperty: #Geometry name: 'Margin Top' as: #asInteger getter: #marginTop model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds marginTop: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> maxHeight: anObject [

	self layout maxHeight: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> maxWidth: anObject [

	self layout maxWidth: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> minHeight: anObject [

	self layout minHeight: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> minWidth: anObject [

	self layout minWidth: anObject
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> outerBounds [
	"can be used in layouts"
	^ self wrappedBounds outerBounds
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> padding [

	^ self wrappedBounds padding
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> padding: aCollection [
	<brickBuilderProperty: #Geometry name: 'Padding' as: #brickBuilderEvaluate getter: #padding model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds padding: aCollection
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingBottom [

	^ self wrappedBounds paddingBottom
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingBottom: anObject [
	<brickBuilderProperty: #Geometry name: 'Padding Bottom' as: #asInteger getter: #paddingBottom model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds paddingBottom: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingLeft [

	^ self wrappedBounds paddingLeft
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingLeft: anObject [
	<brickBuilderProperty: #Geometry name: 'Padding Left' as: #asInteger getter: #paddingLeft model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds paddingLeft: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingRight [

	^ self wrappedBounds paddingRight
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingRight: anObject [
	<brickBuilderProperty: #Geometry name: 'Padding Right' as: #asInteger getter: #paddingRight model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds paddingRight: anObject
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingTop [

	^ self wrappedBounds paddingTop
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> paddingTop: anObject [
	<brickBuilderProperty: #Geometry name: 'Padding Top' as: #asInteger getter: #paddingTop model: #GLMUIBuilderTextFieldProperty>

	self wrappedBounds paddingTop: anObject
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> position: aPoint [

	self translateBy: (aPoint asNonFractionalPoint - self brickBounds topLeft)
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> privateBounds: aRectangle [
	
	^ self explicitRequirement
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> privateHeight: anObject [
	
	self layout height: anObject
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> privateWidth: anObject [

	self layout width: anObject
]

{ #category : #private-extension }
GLMBrickGeometryTrait >> removeProperty: aSymbol [
	
	^ self explicitRequirement
]

{ #category : #private-extension }
GLMBrickGeometryTrait >> setProperty: aSymbol toValue: anObject [
	
	^ self explicitRequirement
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> translateBy: aPoint [

	self wrappedBounds translateBy: aPoint
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> translateX: anInteger [

	self wrappedBounds translateX: anInteger
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> translateXTo: anInteger [

	self translateX: (self brickBounds left negated + anInteger)
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> translateY: anInteger [

	self wrappedBounds translateY: anInteger
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> translateYTo: anInteger [

	self translateY: (self brickBounds top negated + anInteger)
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> updateFoundationBounds [

	self owner ifNotNil: [ :o | o isFoundationBrick ifTrue: [ 
		self doNotLayoutDuring: [
			(o isVShrinking | o isHShrinking) ifTrue: [ o privateExtent: self outerBounds extent ] ] ] ]
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> updateHeight [
	"no need to care about height, when it's logic is not customized"
	self layout isHeightCustom ifFalse: [ ^ self ].
	[ self bounds: (self brickBounds withHeight: self customHeight) ]
		on: Exception
		do: [ "just skip and do nothing" ]
]

{ #category : #private-geometry }
GLMBrickGeometryTrait >> updateWidth [
	"no need to care about width, when it's logic is not customized"
	self layout isWidthCustom ifFalse: [ ^ self ].
	[ self bounds: (self brickBounds withWidth: self customWidth) ]
		on: Exception
		do: [ "just skip and do nothing" ]
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> vMargin [

	^ self marginTop + self marginBottom
]

{ #category : #brick-geometry-properties }
GLMBrickGeometryTrait >> vPadding [

	^ self paddingTop + self paddingBottom
]

{ #category : #private-extension }
GLMBrickGeometryTrait >> valueOfProperty: aSymbol [
	
	^ self explicitRequirement
]

{ #category : #private-extension }
GLMBrickGeometryTrait >> valueOfProperty: aSymbol ifAbsent: aBlock [
	
	^ self explicitRequirement
]

{ #category : #private-extension }
GLMBrickGeometryTrait >> valueOfProperty: aSymbol ifAbsentPut: aBlock [ 
	
	^ self explicitRequirement
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> width [

	^ self brickBounds width
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> width: anObject [
	<brickBuilderProperty: #Geometry name: 'Width' as: #asInteger getter: #width model: #GLMUIBuilderTextFieldProperty>

	self privateWidth: anObject.
	self updateWidth
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> withHeight: anInteger [

	self wrappedBounds height: anInteger
]

{ #category : #brick-geometry-transformations }
GLMBrickGeometryTrait >> withWidth: anInteger [

	self wrappedBounds width: anInteger
]

{ #category : #brick-geometry }
GLMBrickGeometryTrait >> wrappedBounds [

	^ self explicitRequirement
]
