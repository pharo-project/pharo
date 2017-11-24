"
Action when the method comes from a trait
"
Class {
	#name : #MethodFromTraitAction,
	#superclass : #AbstractMethodIconAction,
	#category : #'Nautilus-Widgets'
}

{ #category : #order }
MethodFromTraitAction >> actionOrder [
	"Return the priority of this action"
	
	^ 200
]

{ #category : #order }
MethodFromTraitAction >> isActionHandled [

	^ method isFromTrait
]

{ #category : #order }
MethodFromTraitAction >> privateActionIcon [
	"Return the icon for this action"

	^ IconicButton new 
			target: method originMethod;
			actionSelector: #browse;
			labelGraphic: (self iconNamed: #traitIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Browse the trait';
			borderWidth: 0
]
