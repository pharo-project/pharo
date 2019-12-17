"
I am a style-class, represented by a name. 
I contain other styles (properties or classes).
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
