"
A TimeProfilerNode represents a node in the tally tree
"
Class {
	#name : #TimeProfilerNode,
	#superclass : #MorphTreeNodeModel,
	#category : 'Tool-Profilers'
}

{ #category : #printing }
TimeProfilerNode >> asString [
	^ String streamContents: [:s | self item printOn: s total: model tally totalTime: model time tallyExact: false] 
]

{ #category : #actions }
TimeProfilerNode >> browseItem [
	Smalltalk tools browser
		openOnClass: self methodClass  selector: self selector
]

{ #category : #accessing-computed }
TimeProfilerNode >> childrenItems [
	^ (self item sonsOver: model threshold) asSortedCollection
]

{ #category : #actions }
TimeProfilerNode >> exploreItem [
	self item inspect
]

{ #category : #actions }
TimeProfilerNode >> inspectItem [
	self item inspect
]

{ #category : #'UI specific' }
TimeProfilerNode >> keyStroke: anEvent from: aTreeMorph [
	| c |
	c := anEvent keyCharacter.
	c = $b
		ifTrue: [self browseItem. ^ true].
	c = $i
		ifTrue: [self inspectItem. ^ true].
	c = $I
		ifTrue: [self exploreItem. ^ true].
	^ false

]

{ #category : #'UI specific' }
TimeProfilerNode >> menu: menu shifted: b [ 
	menu
		add: 'Browse (b)'
		target: self
		selector: #browseItem.
	menu
		add: 'Inspect (i)'
		target: self
		selector: #inspectItem.
	menu
		add: 'Explore (I)'
		target: self
		selector: #exploreItem.

]

{ #category : #accessing-computed }
TimeProfilerNode >> methodClass [
	^self item method methodClass 
	
]

{ #category : #accessing-computed }
TimeProfilerNode >> percentage [ 
	^ self item tally asFloat / model time * 100.0 roundTo: 0.1.

]

{ #category : #accessing-computed }
TimeProfilerNode >> selector [
	^self item method selector 
	
]
