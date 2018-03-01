Extension { #name : #RBBrowserEnvironment }

{ #category : #'*NautilusCommon' }
RBBrowserEnvironment >> asSystemNavigationEnvironment [

	| env globalsNames |
	"Optimization for the default environment AKA Smalltalk"
	self class == RBBrowserEnvironment 
		ifTrue: [ ^ Smalltalk globals ].
	
	env := SystemDictionary new.
	
	self classesDo: [:each | 
		env add: ( Smalltalk globals associationAt: each instanceSide name )].
	self traitsDo:  [:each | 
		env add: ( Smalltalk globals associationAt: each name )].
	globalsNames := Smalltalk globals keys asOrderedCollection.
	globalsNames removeAll: ( Smalltalk globals classNames ).
	 globalsNames removeAll: ( Smalltalk globals traitNames ).
	globalsNames do: [:each | 
		env add: ( Smalltalk globals associationAt: each )].

	^ env
]

{ #category : #'*NautilusCommon' }
RBBrowserEnvironment >> traits [
	| traits |
	traits := IdentitySet new: 4096.
	self traitsDo: [ :each | traits add: each ].
	^ traits
]

{ #category : #'*NautilusCommon' }
RBBrowserEnvironment >> traitsDo: aBlock [

	self systemDictionary allTraitsDo: [ :each | 
		(self includesClass: each)
			ifTrue: [ aBlock value: each ]  ]
]
