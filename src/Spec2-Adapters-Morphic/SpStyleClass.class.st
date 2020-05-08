"
! Style classes

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
	#name : #SpStyleClass,
	#superclass : #SpStyle,
	#instVars : [
		'parent',
		'name',
		'styles'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #copying }
SpStyleClass >> , joinStyle [

	^ self class new
		styles: self styles, joinStyle styles;
		yourself
]

{ #category : #operations }
SpStyleClass >> applyTo: aMorph [

	self properties do: [ :each | 
		each applyTo: aMorph ]
]

{ #category : #accessing }
SpStyleClass >> flattenClasses [

	^ (self styles
		inject: (OrderedCollection with: self)
		into: [ :all :each | 
			each isStyleClass ifTrue: [ all addAll: each flattenClasses ].
			all ])
		flattened
]

{ #category : #accessing }
SpStyleClass >> flattenProperties [

	^ (self flattenClasses
		inject: self properties
		into: [ :all :each | all, each properties ])
		flattened
]

{ #category : #initialization }
SpStyleClass >> fromSton: stonReader [
	
	styles := stonReader parseList
]

{ #category : #accessing }
SpStyleClass >> fullName [

	^ (self parent 
		ifNotNil: [ 
			| parentName |
			parentName := self parent fullName.
			parentName = 'application' 
				ifTrue: [ self name ]
				ifFalse: [ parentName, '.', self name ] ]
		ifNil: [ self name ])
]

{ #category : #testing }
SpStyleClass >> isStyleClass [

	^ true
]

{ #category : #accessing }
SpStyleClass >> name [

	^ name
]

{ #category : #accessing }
SpStyleClass >> name: aString [ 
	
	name := aString
]

{ #category : #accessing }
SpStyleClass >> parent [ 

	^ parent
]

{ #category : #accessing }
SpStyleClass >> parent: aStyle [

	parent := aStyle
]

{ #category : #printing }
SpStyleClass >> printOn: stream [ 

	super printOn: stream.
	stream << '(' << self name << ')'
]

{ #category : #accessing }
SpStyleClass >> properties [

	^ self styles select: [ :each | each isStyleProperty ]
]

{ #category : #accessing }
SpStyleClass >> styles [

	^ styles
]

{ #category : #private }
SpStyleClass >> styles: aCollection [

	styles := aCollection asArray
]
