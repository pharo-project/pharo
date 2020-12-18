"
Serializes a portion of the current stack trace using fuel.
"
Class {
	#name : #StFuelOutStackCommand,
	#superclass : #StDebuggerCommand,
	#category : #'Fuel-Tools-Pharo'
}

{ #category : #testing }
StFuelOutStackCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #actions }
StFuelOutStackCommand >> execute [
	self serializeStack
]

{ #category : #actions }
StFuelOutStackCommand >> serializeStack [
	| date fileName |
	"Serialize a portion of the current stack trace using fuel.."
	date := DateAndTime now.
	
	fileName := String streamContents: [ :s|
		s 
			<< 'Debugger-Stack-';
			<< self interruptedContext receiver class instanceSide name;
			<< '-'.
		date printYMDOn: s.
		s 
			<< '-'; print: date hour24; print: date minute; print: date second;
			<< '.fuel'].

	self class
		serializeTestFailureContext: self interruptedContext copyStack		
		toFileNamed: fileName
]
