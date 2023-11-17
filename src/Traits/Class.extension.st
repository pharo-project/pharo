Extension { #name : 'Class' }

{ #category : '*Traits' }
Class >> setTraitComposition: aTraitComposition [

	^ self classInstaller
		  update: self
		  to: [ :builder | builder traitComposition: aTraitComposition ]
]

{ #category : '*Traits' }
Class >> usesTrait: aTrait [
	"Returns whether self or one of its superclasses are among the users of aTrait"
	^ aTrait users includesAny: self withAllSuperclasses
]

{ #category : '*Traits' }
Class >> usesTraitLocally: aTrait [
	"Returns whether self is among the users of aTrait. Note that this will return false if aTrait is used by a superclass of self and not self itself. If you want to get true in this case, use #usesTrait:"
	^ aTrait users includes: self
]
