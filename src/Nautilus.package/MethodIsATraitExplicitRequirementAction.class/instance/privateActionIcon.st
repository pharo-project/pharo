order
privateActionIcon
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