"
I represent the root element in a TraitComposition.
I wrap a metaclass or classTrait to be used as a trait in a trait composition.
"
Class {
	#name : 'TaClassCompositionElement',
	#superclass : 'TaCompositionElement',
	#category : 'Traits-Compositions',
	#package : 'Traits',
	#tag : 'Compositions'
}

{ #category : 'accessing' }
TaClassCompositionElement >> methods [
	"As I am representing a ClassTrait I have to filter the methods that are in all the class traits"

	| innerClassLocalMethods traitedClassSelectors |
	innerClassLocalMethods := innerClass localMethods collect: [ :each | each selector ].
	traitedClassSelectors := TraitedClass selectors.
	^ super methods reject: [ :method |
		  | methodSelector |
		  methodSelector := method selector.
		  (innerClassLocalMethods includes: methodSelector) not and: [ traitedClassSelectors anySatisfy: [ :x | x = methodSelector ] ] ]
]

{ #category : 'accessing' }
TaClassCompositionElement >> selectorsDo: aBlock [
	"As I am representing a ClassTrait I have to filter the methods that are in all the class traits"

	| selectorsToExclude block |
	selectorsToExclude := TraitedClass selectors \ innerClass localSelectors.

	block := [ :selector |
		         | actualSelector |
		         actualSelector := self actualSelector: selector.

		         (selectorsToExclude includes: selector) ifFalse: [ aBlock value: actualSelector ] ].
	
	"We do not rely on the methods of the innerclass because while removing a trait we might return the removed methods during the removal and I need to work properly during the method dictionary rebuild"
	innerClass localSelectors do: block.
	innerClass traitComposition selectorsDo: block
]

{ #category : 'accessing' }
TaClassCompositionElement >> slots [
	^ innerClass allSlots
		reject: [ :e |
			(Trait slots ,
			TraitedClass allSlots) anySatisfy: [ :x | x name = e name ] ]
]

{ #category : 'printing' }
TaClassCompositionElement >> traitCompositionExpression [

	^ innerClass instanceSide name , ' classTrait'
]
