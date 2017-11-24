"
I show an action for methods with breakpoints in the browser
"
Class {
	#name : #MethodContainsBreakAction,
	#superclass : #AbstractMethodIconAction,
	#category : #'Nautilus-Widgets'
}

{ #category : #order }
MethodContainsBreakAction >> actionOrder [
	"Return the priority of this action"
	
	^ 90
]

{ #category : #order }
MethodContainsBreakAction >> isActionHandled [

	^ method hasBreakpoint
]

{ #category : #order }
MethodContainsBreakAction >> privateActionIcon [
	"Return the icon for this action"
	^ IconicButton new
		target: self browser;
		actionSelector: #toggleBreakpointIn:;
		arguments: { self method};
		labelGraphic: (self iconNamed: #haltIcon);
		color: Color transparent;
		helpText: 'remove breakpoint';
		extent: self iconExtent;
		borderWidth: 0
]
