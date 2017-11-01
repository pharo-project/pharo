"
Action when the method overrides another one, or the method is overidden by another one or both
"
Class {
	#name : #MethodOveridesAction,
	#superclass : #AbstractMethodIconAction,
	#instVars : [
		'isOverridden',
		'isOverride',
		'result'
	],
	#category : 'Nautilus-Widgets'
}

{ #category : #order }
MethodOveridesAction >> actionOrder [
	"Return the priority of this action"
	
	^ 600
]

{ #category : #order }
MethodOveridesAction >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	^ result 
		ifNil: [ super actionStateToCache ]
		ifNotNil: [ {(IconicButtonStateHolder forNautilus: result value first). (IconicButtonStateHolder forNautilus: result value second)} ]
]

{ #category : #private }
MethodOveridesAction >> buildUpAndDownArrowIcon: aMethod [

	| container up down |
	container := Morph new.
	container 
		extent: 12@12;
		color: Color transparent.
	up := IconicButton new 
				target: self browser;
				actionSelector: #arrowUp:;
				arguments: { aMethod };
				labelGraphic: (self iconNamed: #arrowDoubleUpIcon);
				color: Color transparent;
				extent: 16 @ 8;
				helpText: 'Browse overriden message';
				borderWidth: 0.
	down :=  IconicButton new 
				target: self browser;
				actionSelector: #arrowDown:;
				arguments: { aMethod };
				labelGraphic: (self iconNamed: #arrowDoubleDownIcon);
				color: Color transparent;
				extent: 16 @ 8;
				helpText: 'Browse overriding messages';
				borderWidth: 0.
	^ (container
		changeTableLayout;
		listDirection: #topToBottom;
		addMorph: down;
		addMorph: up;
		yourself) -> {up. down}.
]

{ #category : #order }
MethodOveridesAction >> isActionHandled [

	isOverridden := self isOverridden.
	isOverride := self isOverride.
	^ isOverridden or: [ isOverride ]
]

{ #category : #order }
MethodOveridesAction >> isOverridden [
	| selector| 
	selector := method selector.
	method methodClass allSubclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
	
]

{ #category : #order }
MethodOveridesAction >> isOverride [
	| selector |
	selector := method selector.
	method methodClass allSuperclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
]

{ #category : #order }
MethodOveridesAction >> privateActionIcon [
	"Return the icon for this action"

	isOverride
		ifTrue: [ 
			isOverridden
				ifTrue: [
					result := self buildUpAndDownArrowIcon: method.
					^ result key ]
				ifFalse: [ 
					^ IconicButton new
						target: self browser;
						actionSelector: #arrowUp:;
						arguments: {method};
						labelGraphic: (self iconNamed: #arrowUpIcon);
						color: Color transparent;
						helpText: 'Browse overriden message';
						extent: self iconExtent;
						borderWidth: 0 ] ]
		ifFalse: [ 
			isOverridden
				ifTrue: [ 
					^ IconicButton new
						target: self browser;
						actionSelector: #arrowDown:;
						arguments: {method};
						labelGraphic: (self iconNamed: #arrowDownIcon);
						color: Color transparent;
						helpText: 'Browse overriding messages';
						extent: self iconExtent;
						borderWidth: 0 ] ]
]
