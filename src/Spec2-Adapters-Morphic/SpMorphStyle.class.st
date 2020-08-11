"
! A Style in a Morph (SpMorphStyle)

This is for internal use of the framework, but it is interesting to notice how it works since it may give some insigts on how to declare things.
At creation of a component, an instance of ==SpMorphStyle== is created and by taking the stylesheet it collects styles from most external to more internal. So, for example, this stylesheet: 

[[[
.application [
	Geometry { #height: 100 }, 
	.button [
		Geometry { #width: 100 }
	]
]
]]]

Will collect, for a button (*class:SpButtonPresenter*, who has a style name ==button==), the styles

[[[
application
application.button
]]] 

This collection will be used to get all properties defined and perform a merge between them (*method:SpStyleProperty#mergeWith:*), to get all one single property for each type of them. Which means at the end it will apply a property ==Geometry { #width: 100, #height: 25 }==.

"
Class {
	#name : #SpMorphStyle,
	#superclass : #Object,
	#instVars : [
		'styles',
		'mergedProperties'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'instance creation' }
SpMorphStyle class >> fromStylesheet: aStyle adapter: anAdapter [
	"I collect all styles that apply to an adapter.
	 I traverse all styles in a very precise order: from more generic to more specific, this way 
	 the order of the collected elements will be in reverse order of execution."
	| allStyleClasses styles |
		
	allStyleClasses := aStyle flattenClasses.
	styles := OrderedCollection new.
	(({ 'application'. anAdapter styleName }, 
		anAdapter presenterStyles, 
		(anAdapter presenterStyles collect: [ :each | anAdapter styleName, '.', each ])) 
		do: [ :each |
			"a hack to not do a double dispatch on string :(" 
			each isString 
				ifTrue: [ 
					allStyleClasses 
						select: [ :eachStyle | eachStyle fullName = each ]
						thenDo: [ :aStyleClass | styles add: aStyleClass ] ] 
				ifFalse: [ 
					styles add: each ] ]).
	"WARNING. Order is important"
	^ self newStyles: styles asArray
]

{ #category : #'instance creation' }
SpMorphStyle class >> newStyles: anArray [

	^ self new 
		styles: anArray;
		yourself
]

{ #category : #accessing }
SpMorphStyle >> applyTo: aMorph [ 

	aMorph setProperty: #style toValue: self.
	self mergedProperties do: [ :each |
		each applyTo: aMorph ]
]

{ #category : #'accessing container' }
SpMorphStyle >> borderWidth [

	^ self lookupProperty: SpStyleContainer valueOf: #borderWidth
]

{ #category : #private }
SpMorphStyle >> calculateMergedProperties [
	| properties groups |
	
	properties := OrderedCollection new.
	groups := (self styles 
		flatCollect: [ :each | each properties ])
		groupedBy: [ :each | each class ].
		
	groups valuesDo: [ :eachCollection |
		properties add: (eachCollection allButFirst 
			inject: eachCollection first
			into: [ :all :each | all, each ]) ].
	
	^ properties
]

{ #category : #'accessing geometry' }
SpMorphStyle >> extent [

	^ self lookupProperty: SpStyleGeometry valueOf: #extent
]

{ #category : #'accessing font' }
SpMorphStyle >> font [

	^ self lookupProperty: SpStyleFont valueOf: #definedFont
]

{ #category : #'accessing geometry' }
SpMorphStyle >> height [

	^ self lookupProperty: SpStyleGeometry valueOf: #height
]

{ #category : #accessing }
SpMorphStyle >> lookupProperty: aPropertyClass valueOf: aSymbol [
	"I perform the lookup of a property, for which I need a class and a property name.
	 I will traverse all collected properties from last to first, to ensure most detailed 
	 property is answered"

	(self propertiesLike: aPropertyClass)
		reverseDo: [ :each | 
			(each perform: aSymbol) 
				ifNotNil: [ :value | ^ value ] ].
	^ nil
]

{ #category : #'accessing geometry' }
SpMorphStyle >> maxHeight [

	^ self lookupProperty: SpStyleGeometry valueOf: #maxHeight
]

{ #category : #'accessing geometry' }
SpMorphStyle >> maxWidth [

	^ self lookupProperty: SpStyleGeometry valueOf: #maxWidth
]

{ #category : #accessing }
SpMorphStyle >> mergedProperties [

	^ mergedProperties ifNil: [ mergedProperties := self calculateMergedProperties ]
]

{ #category : #'accessing geometry' }
SpMorphStyle >> minHeight [

	^ self lookupProperty: SpStyleGeometry valueOf: #minHeight
]

{ #category : #'accessing geometry' }
SpMorphStyle >> minWidth [

	^ self lookupProperty: SpStyleGeometry valueOf: #minWidth
]

{ #category : #'accessing container' }
SpMorphStyle >> padding [ 

	^ self lookupProperty: SpStyleContainer valueOf: #padding
]

{ #category : #private }
SpMorphStyle >> propertiesLike: aClass [

	^ (self styles 
		inject: OrderedCollection new
		into: [ :all :eachStyle | 
			eachStyle properties 
				select: [ :eachProperty | eachProperty class = aClass ]
				thenDo: [ :eachProperty | all add: eachProperty ].
			all ])
		flattened
]

{ #category : #accessing }
SpMorphStyle >> styles [
	^ styles
]

{ #category : #accessing }
SpMorphStyle >> styles: anObject [
	styles := anObject
]

{ #category : #'accessing geometry' }
SpMorphStyle >> width [

	^ self lookupProperty: SpStyleGeometry valueOf: #width
]
