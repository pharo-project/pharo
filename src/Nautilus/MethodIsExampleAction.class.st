"
Action when the method is an example
"
Class {
	#name : #MethodIsExampleAction,
	#superclass : #AbstractMethodIconAction,
	#category : #'Nautilus-Widgets'
}

{ #category : #order }
MethodIsExampleAction >> actionOrder [
	"Return the priority of this action"
	
	^0
]

{ #category : #order }
MethodIsExampleAction >> isActionHandled [

	^  (method methodClass isMeta) and: [
			method numArgs isZero and: [   
				(method selector beginsWith: 'example') or: [ 
					method hasPragmaNamed: #example 
		]]]
	 
]

{ #category : #order }
MethodIsExampleAction >> privateActionIcon [

	 ^IconicButton new 
			target: self browser;
			actionSelector: #runExampleMethod:;
		 	arguments: { method };
			labelGraphic: (self iconNamed: #smallDoItIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Run the example';
			borderWidth: 0 
]
