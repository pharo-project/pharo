"
I represent the root element in a TraitComposition.
I wrap a metaclass or classTrait to be used as a trait in a trait composition.
"
Class {
	#name : #TaClassCompositionElement,
	#superclass : #TaCompositionElement,
	#category : #'TraitsV2-Compositions'
}

{ #category : #accessing }
TaClassCompositionElement >> methods [
	"As I am representing a ClassTrait I have to filter the methods that are in all the class traits"
	| innerClassLocalMethods |
	innerClassLocalMethods := innerClass localMethods collect: #selector.
	^ super methods reject: [ :e | (innerClassLocalMethods includes: e selector) not and: [TraitedClass selectors anySatisfy: [ :x | x = e selector ]] ]
]

{ #category : #accessing }
TaClassCompositionElement >> selectors [
	"As I am representing a ClassTrait I have to filter the methods that are in all the class traits"

	| innerClassLocalMethods |
	innerClassLocalMethods := innerClass localMethods collect: #selector.
	^ super selectors
		reject: [ :e | 
			(innerClassLocalMethods includes: e) not
				and: [ TraitedClass selectors includes: e ] ]
]

{ #category : #accessing }
TaClassCompositionElement >> slots [
	^ innerClass allSlots
		reject: [ :e | 
			(Trait slots , 
			TraitedClass allSlots) anySatisfy: [ :x | x name = e name ] ]
		
]

{ #category : #printing }
TaClassCompositionElement >> traitCompositionExpression [
	
	^ innerClass instanceSide name , ' classTrait'
]
