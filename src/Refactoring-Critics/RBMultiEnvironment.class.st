Class {
	#name : #RBMultiEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'environmentDictionaries'
	],
	#category : #Refactoring-Critics
}

{ #category : #adding }
RBMultiEnvironment >> addClass: aClass into: aValue [ 
	(environmentDictionaries at: aValue ifAbsentPut: [RBSelectorEnvironment new]) 
		addClass: aClass
]

{ #category : #adding }
RBMultiEnvironment >> addClass: aClass selector: aSymbol into: aValue [ 
	(environmentDictionaries at: aValue ifAbsentPut: [RBSelectorEnvironment new]) 
		addClass: aClass
		selector: aSymbol
]

{ #category : #accessing }
RBMultiEnvironment >> asSelectorEnvironment [
	| s |
	s := RBSelectorEnvironment new.
	s label: self label.
	environmentDictionaries do: [:each | | env |
			env := each asSelectorEnvironment. 
			env classesDo: [:cls | env selectorsForClass: cls do:  [:sel | s addClass: cls selector: sel]]].
	^ s
	  
]

{ #category : #accessing }
RBMultiEnvironment >> environments [
	^environmentDictionaries keys
]

{ #category : #testing }
RBMultiEnvironment >> includesCategory: aCategory [
	^ (super includesCategory: aCategory) and: [ environmentDictionaries anySatisfy: [ :env | env includesCategory: aCategory ] ]
]

{ #category : #testing }
RBMultiEnvironment >> includesClass: aClass [
	^ (super includesClass: aClass) and: [ environmentDictionaries anySatisfy: [ :env | env includesClass: aClass ] ]
]

{ #category : #testing }
RBMultiEnvironment >> includesProtocol: aProtocol in: aClass [
	^ (super includesProtocol: aProtocol in: aClass) and: [ environmentDictionaries anySatisfy: [ :env | env includesProtocol: aProtocol in: aClass ] ]
]

{ #category : #testing }
RBMultiEnvironment >> includesSelector: aSelector in: aClass [
	^ (super includesSelector: aSelector in: aClass) and: [ environmentDictionaries anySatisfy: [ :env | env includesSelector: aSelector in: aClass ] ]
]

{ #category : #initialization }
RBMultiEnvironment >> initialize [
	super initialize.
	environmentDictionaries := Dictionary new.
	environment := RBSelectorEnvironment new
]

{ #category : #testing }
RBMultiEnvironment >> isEmpty [
	^environmentDictionaries isEmpty
]

{ #category : #accessing }
RBMultiEnvironment >> packages [
	self shouldBeImplemented
]

{ #category : #accessing }
RBMultiEnvironment >> problemCount [
	^environmentDictionaries size
]

{ #category : #removing }
RBMultiEnvironment >> removeClass: aClass [
	environmentDictionaries keysDo: [ :key |
		(environmentDictionaries at: key) removeClass: aClass ]
]

{ #category : #removing }
RBMultiEnvironment >> removeClass: aClass into: aValue [ 
	(environmentDictionaries at: aValue ifAbsent: [RBSelectorEnvironment new]) 
		removeClass: aClass
]

{ #category : #removing }
RBMultiEnvironment >> removeClass: aClass selector: aSelector into: aValue [ 
	(environmentDictionaries at: aValue ifAbsentPut: [RBSelectorEnvironment new]) 
		removeClass: aClass
		selector: aSelector
]

{ #category : #accessing }
RBMultiEnvironment >> selectEnvironment: aValue [ 
	environment := environmentDictionaries at: aValue
				ifAbsent: [RBSelectorEnvironment new]
]
