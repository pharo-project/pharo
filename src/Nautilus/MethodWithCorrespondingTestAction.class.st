"
Action when the method has a corresponding test method
"
Class {
	#name : #MethodWithCorrespondingTestAction,
	#superclass : #AbstractMethodIconAction,
	#instVars : [
		'testMethod'
	],
	#category : #'Nautilus-Widgets'
}

{ #category : #order }
MethodWithCorrespondingTestAction >> actionOrder [
	"Return the priority of this action"
	
	^ 500
]

{ #category : #order }
MethodWithCorrespondingTestAction >> isActionHandled [

	^ (testMethod := method correspondingTestMethod) notNil 
]

{ #category : #order }
MethodWithCorrespondingTestAction >> privateActionIcon [
	"Return the icon for this action"
	
	^ IconicButton new
		target: self browser;
		actionSelector: #runTestForAMethod:notifying:;
		arguments:  { method. true};
		labelGraphic: (self iconForTestResultOf: testMethod) ;
		color: Color transparent;
		extent: 12 @ 12;
		helpText: 'Run the test';
		borderWidth: 0
]
