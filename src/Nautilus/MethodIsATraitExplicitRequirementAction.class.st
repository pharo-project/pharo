"
Action when the method is a explicitRequired
"
Class {
	#name : #MethodIsATraitExplicitRequirementAction,
	#superclass : #AbstractMethodIconAction,
	#category : 'Nautilus-Widgets'
}

{ #category : #order }
MethodIsATraitExplicitRequirementAction >> actionOrder [
	^150.
]

{ #category : #order }
MethodIsATraitExplicitRequirementAction >> isActionHandled [
	^method isRequired and: [ method methodClass ~= method origin ]
]

{ #category : #order }
MethodIsATraitExplicitRequirementAction >> privateActionIcon [
	"Return the icon for this action"
	"^ Smalltalk ui icons iconNamed: #traitRequiredMethodIcon"
	^ IconicButton new 
			target: method originMethod;
			actionSelector: #browse;
			labelGraphic: (self iconNamed: #traitRequiredMethodIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Browse the trait';
			borderWidth: 0
]
