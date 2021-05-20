"
! Style classes (==SpStyleClass==)

A style class define a set of properties grouped by a common name. You can think a style class of morphic a little bit as a style class of CSS, but it has several differences.
 
!! Style classes can be nested
You can nest classes to refine some properties. For example, if you have this definition: 

[[[
.application [
	.button [
		Geometry { #height: 25, #width: 100 }
		.smallButton {
			Geometry { #width: 150 }
		}
	]	
]
]]]

the result style for a button with ""smallButton"" style will have a Geometry with the form: ==Geometry { #width: 150, #height: 25 }==, which is the result of the merge of all properties, with the deepest nested property taking precedence.

!! Style classes are composable
You can compose class styles (stacking them to form a new style). This is an useful practice to add your own styles to the default definition.  

[[[language=Smalltalk  
myStyle := SpStyle defaultStyleSheet, myOwnStyleDefinition			
]]]
"
Class {
	#name : #SpClassStyle,
	#superclass : #SpStyle,
	#instVars : [
		'parent',
		'name',
		'styles'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #copying }
SpClassStyle >> , joinStyle [

	^ self mergeWith: joinStyle
]

{ #category : #adding }
SpClassStyle >> addClass: aName with: aBlock [
	| class |
	
	class := self class new
		parent: self;
		name: aName;
		yourself.
	
	aBlock value: class.
	self addStyle: class
]

{ #category : #adding }
SpClassStyle >> addProperty: aClass with: aBlock [ 
	| property |
	
	property := aClass new.
	aBlock value: property.
	self addStyle: property
]

{ #category : #adding }
SpClassStyle >> addPropertyContainerWith: aBlock [ 
	
	self addProperty: SpContainerStyle with: aBlock
]

{ #category : #adding }
SpClassStyle >> addPropertyDrawWith: aBlock [ 
	
	self addProperty: SpDrawStyle with: aBlock
]

{ #category : #adding }
SpClassStyle >> addPropertyFontWith: aBlock [ 
	
	self addProperty: SpFontStyle with: aBlock
]

{ #category : #adding }
SpClassStyle >> addPropertyGeometryWith: aBlock [ 
	
	self addProperty: SpGeometryStyle with: aBlock
]

{ #category : #adding }
SpClassStyle >> addPropertyTextWith: aBlock [ 
	
	self addProperty: SpTextStyle with: aBlock
]

{ #category : #adding }
SpClassStyle >> addStyle: aStyle [

	styles ifNil: [ styles := #() ].
	styles := styles copyWith: aStyle
]

{ #category : #operations }
SpClassStyle >> applyTo: aMorph [

	self properties do: [ :each | 
		each applyTo: aMorph ]
]

{ #category : #accessing }
SpClassStyle >> flattenClasses [

	^ (self styles
		inject: (OrderedCollection with: self)
		into: [ :all :each | 
			each isStyleClass ifTrue: [ all addAll: each flattenClasses ].
			all ])
		flattened
]

{ #category : #accessing }
SpClassStyle >> flattenProperties [

	^ (self flattenClasses
		inject: self properties
		into: [ :all :each | all, each properties ])
		flattened
]

{ #category : #initialization }
SpClassStyle >> fromSton: stonReader [
	
	styles := stonReader parseList
]

{ #category : #accessing }
SpClassStyle >> fullName [

	^ (self parent 
		ifNotNil: [ 
			| parentName |
			parentName := self parent fullName.
			parentName = 'application' 
				ifTrue: [ self name ]
				ifFalse: [ parentName, '.', self name ] ]
		ifNil: [ self name ])
]

{ #category : #initialization }
SpClassStyle >> initialize [

	super initialize.
	styles := #()
]

{ #category : #testing }
SpClassStyle >> isStyleClass [

	^ true
]

{ #category : #copying }
SpClassStyle >> mergeWith: joinStyle [

	^ self class new
		name: name;
		styles: (self mergedStylesWith: joinStyle);
		yourself
]

{ #category : #private }
SpClassStyle >> mergedStylesWith: anotherStyle [

	| allStyles results|
	
	results := OrderedCollection new.
	
	allStyles := (self styles , anotherStyle styles) 
		groupedBy: [:e | e isStyleClass ifTrue: [ e name ] ifFalse: [ e class]].
	
	allStyles valuesDo: [ :value | 
		results add: (value allButFirst inject: value first into: [:accum :each | accum mergeWith: each ])].
	
	^ results asArray
]

{ #category : #accessing }
SpClassStyle >> name [

	^ name
]

{ #category : #accessing }
SpClassStyle >> name: aString [ 
	
	name := aString
]

{ #category : #accessing }
SpClassStyle >> parent [ 

	^ parent
]

{ #category : #accessing }
SpClassStyle >> parent: aStyle [

	parent := aStyle
]

{ #category : #printing }
SpClassStyle >> printOn: stream [ 

	super printOn: stream.
	stream << '(' << self fullName << ')'
]

{ #category : #accessing }
SpClassStyle >> properties [

	^ self styles select: [ :each | each isStyleProperty ]
]

{ #category : #accessing }
SpClassStyle >> styles [

	^ styles
]

{ #category : #private }
SpClassStyle >> styles: aCollection [

	styles := aCollection asArray
]
