"
Traits (unit of methods) can be composed out of other traits using + between traits, - to exclude a method and an alias @ to give another name to a method (this is not a rename just an alias).
I'm the common superclass for all the implied nodes.
"
Class {
	#name : #CDAbstractTraitCompositionNode,
	#superclass : #CDNode,
	#category : #'ClassParser-Model'
}

{ #category : #composing }
CDAbstractTraitCompositionNode >> + aNode [
	
	^ CDTraitCompositionSequenceNode new
		sequence: { self . aNode }
]

{ #category : #combining }
CDAbstractTraitCompositionNode >> - aCollection [ 
	
	^ CDTraitExclusionNode new
		subject: self;
		exclusions: aCollection
]

{ #category : #composing }
CDAbstractTraitCompositionNode >> @ aCollection [ 
	
	^ CDTraitAliasNode new
		subject: self;
		aliases: aCollection
]

{ #category : #testing }
CDAbstractTraitCompositionNode >> isTraitComposition [
	
	^ true
]

{ #category : #testing }
CDAbstractTraitCompositionNode >> isTraitNode [
	^false
]

{ #category : #selection }
CDAbstractTraitCompositionNode >> start [
	
	^ originalNode start
]

{ #category : #selection }
CDAbstractTraitCompositionNode >> stop [
	
	^ originalNode stop
]
