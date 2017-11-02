"
Action when the method is a class initialize method
"
Class {
	#name : #MethodClassInitializeAction,
	#superclass : #AbstractMethodIconAction,
	#category : 'Nautilus-Widgets'
}

{ #category : #order }
MethodClassInitializeAction >> actionOrder [
	"Return the priority of this action"
	
	^0
]

{ #category : #order }
MethodClassInitializeAction >> isActionHandled [

	^ (method methodClass isMeta) and: [ 
		method selector = #initialize ] 
]

{ #category : #order }
MethodClassInitializeAction >> privateActionIcon [

	 ^IconicButton new 
			target: self browser;
			actionSelector: #runClassInitializeMethod:;
		 	arguments: { method };
			labelGraphic: (self iconNamed: #scriptManagerIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: '(Re)initialize class';
			borderWidth: 0 
]
