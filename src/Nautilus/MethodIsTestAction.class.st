"
Aciton when the method is a test
"
Class {
	#name : #MethodIsTestAction,
	#superclass : #AbstractMethodIconAction,
	#category : #Nautilus-Widgets
}

{ #category : #order }
MethodIsTestAction >> actionOrder [
	"Return the priority of this action"
	
	^ 0
]

{ #category : #order }
MethodIsTestAction >> isActionHandled [

	^ method isTestMethod and: [ method methodClass isAbstract not ]
]

{ #category : #order }
MethodIsTestAction >> privateActionIcon [
	"Return the icon for this action"
	
	^ IconicButton new
		target: self browser;
		actionSelector: #runTestForAMethod:notifying:;
		arguments: {method. true};
		labelGraphic: (self iconForTestResultOf: method);
		color: Color transparent;
		helpText: 'Run the test';
		extent: self iconExtent;
		borderWidth: 0
]
