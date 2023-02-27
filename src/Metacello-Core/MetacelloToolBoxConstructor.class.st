Class {
	#name : #MetacelloToolBoxConstructor,
	#superclass : #MetacelloAbstractVersionConstructor,
	#instVars : [
		'currentSection',
		'methodSections'
	],
	#category : #'Metacello-Core-Constructors'
}

{ #category : #private }
MetacelloToolBoxConstructor >> evaluateMethodSection: methodSection version: sourceVersionString [
	| versionSpec |
	versionSpec := self project versionSpec.
	versionSpec versionString: sourceVersionString.
	methodSection versionSpec: versionSpec.
	currentSection := methodSection.
	self with: versionSpec during: methodSection block.
	methodSection methodSections do: [ :ms | self evaluateMethodSection: ms version: sourceVersionString ]
]

{ #category : #extraction }
MetacelloToolBoxConstructor >> extractMethodSectionsFor: sourceVersionString [
	| coll pragma |
	coll := self extractAllVersionPragmas at: sourceVersionString ifAbsent: [ ^ #() ].
	coll size > 1
		ifTrue: [ self error: 'More than one pragma defining ' , sourceVersionString printString ].
	pragma := coll at: 1.
	self evaluatePragma: pragma.
	self methodSections do: [ :methodSection | self evaluateMethodSection: methodSection version: sourceVersionString ].
]

{ #category : #extraction }
MetacelloToolBoxConstructor >> extractSymbolicVersionSpecsFor: sourceVersionSymbol [
	| coll pragma |
	coll := self extractSymbolicVersionPragmas at: sourceVersionSymbol ifAbsent: [ ^ #() ].
	coll size > 1
		ifTrue: [ self error: 'More than one pragma defining ' , sourceVersionSymbol printString ].
	pragma := coll at: 1.
	self evaluatePragma: pragma.
	^ self methodSections
]

{ #category : #private }
MetacelloToolBoxConstructor >> methodSection: methodSection do: aBlock [

	methodSection methodSections do: aBlock.
	methodSection methodSections do: [ :ms | self methodSection: ms do: aBlock ]
]

{ #category : #private }
MetacelloToolBoxConstructor >> methodSection: methodSection inEvaluationOrder: attributes do: aBlock [
    | selected |
    selected := IdentitySet new.
    attributes
        do: [ :attribute | 
            methodSection methodSections
                do: [ :ms | 
                    (ms attributes includes: attribute)
                        ifTrue: [ selected add: ms ] ] ].
    selected do: aBlock.
    attributes size == 1
        ifTrue: [ ^ self ].
    selected
        do: [ :ms | self methodSection: ms inEvaluationOrder: (attributes copyFrom: 2 to: attributes size) do: aBlock ]
]

{ #category : #accessing }
MetacelloToolBoxConstructor >> methodSectionAttributes [
    | attributes |
    attributes := Set new.
    self methodSectionsDo: [ :methodSection | attributes addAll: methodSection attributes ].
    ^ attributes
]

{ #category : #private }
MetacelloToolBoxConstructor >> methodSections [

	methodSections == nil ifTrue: [ methodSections := OrderedCollection new ].
	^methodSections
]

{ #category : #enumeration }
MetacelloToolBoxConstructor >> methodSectionsDo: aBlock [

	self methodSection: self do: aBlock
]

{ #category : #enumeration }
MetacelloToolBoxConstructor >> methodSectionsInEvaluationOrder: attributes do: aBlock [
    "breadth first traversal ... to collect selected sections, then evaluate individual sections in attribute order"

    | selected processed |
    selected := IdentitySet new.
    self methodSection: self inEvaluationOrder: attributes do: [ :methodSection | selected add: methodSection ].
    processed := IdentitySet new.
    attributes
        do: [ :attribute | 
            | list |
            list := OrderedCollection new.
            selected
                do: [ :methodSection | 
                    (processed includes: methodSection)
                        ifFalse: [ 
                            (methodSection attributes includes: attribute)
                                ifTrue: [ 
                                    list add: methodSection.
                                    processed add: methodSection ] ] ].
            list do: aBlock ]
]

{ #category : #initialization }
MetacelloToolBoxConstructor >> reset [
	super reset.	"not needed, but included for completeness"
	methodSections := nil
]

{ #category : #'api callbacks' }
MetacelloToolBoxConstructor >> setFor: attributeList do: aBlock [
    "conditional version support"

    | methodSection |
    methodSection := MetacelloVersionMethodSection new
        attributes: attributeList asMetacelloAttributeList;
        block: aBlock;
        yourself.
    currentSection ~~ nil
        ifTrue: [ currentSection addMethodSection: methodSection ]
        ifFalse: [ self methodSections add: methodSection ]
]

{ #category : #'api callbacks' }
MetacelloToolBoxConstructor >> setFor: attributeList version: aString [
    "conditional symbolicVersion support"

    self methodSections
        add:
            (MetacelloSymbolicVersionSpec new
                attributes: attributeList asMetacelloAttributeList;
                versionString: aString;
                yourself)
]
