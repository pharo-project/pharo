"
I'm a style container that can lookup for properties, following the logic of most detailed property first, most generic last.
At creation, I receive an adapter and a stylesheet, and I will perform a traverse of the stylesheet collecting the styles that applies to the adapted presenter (see method comment).

Once collected the styles, I can lookup a property using the method: #lookupProperty:valueOf:, passing the class and name of property, e.g: 

myStyles 
	lookupProperty: SpStyleGeometry 
	valueOf: #minWidth

For convenience, there are methods which perform the lookups alredy defined (see accessing* protocols).

Finally, an adaptor style can be applied to a morph (using #applyTo:)
"
Class {
	#name : #SpMorphStyle,
	#superclass : #Object,
	#instVars : [
		'styles'
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
		anAdapter presenter styles, 
		(anAdapter presenter styles collect: [ :each | anAdapter styleName, '.', each ])) 
		do: [ :each |
			"a hack to not do a double dispatch on string :(" 
			each isString 
				ifTrue: [ 
					allStyleClasses 
						detect: [ :eachStyle | eachStyle fullName = each ]
						ifFound: [ :aStyleClass | styles add: aStyleClass ] ] 
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
	self styles do: [ :each |
		each applyTo: aMorph ]
]

{ #category : #'accessing container' }
SpMorphStyle >> borderWidth [

	^ self lookupProperty: SpStyleContainer valueOf: #borderWidth
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
