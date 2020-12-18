"
A FuelOutStackDebugAction is	a debugging action that serialize a portion of the current stack trace using fuel.

"
Class {
	#name : #FuelOutStackDebugAction,
	#superclass : #DebugAction,
	#category : #'Fuel-Tools-Pharo-Debugger'
}

{ #category : #registration }
FuelOutStackDebugAction class >> actionType [
	<contextMenuDebuggingAction>
]

{ #category : #private }
FuelOutStackDebugAction class >> encodeDebugInformationOn: aSerializer [
	| str |
	
	str := String new writeStream. 
	str print: Date today; space; print: Time now.
	aSerializer at: #Timestamp putAdditionalObject: str contents. 
	
	str := String new writeStream. 
	str nextPutAll: 'VM: ';
		nextPutAll: Smalltalk os name asString;
		nextPutAll: ' - ';	
		nextPutAll: Smalltalk os subtype asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk os version asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vm version asString.
	aSerializer at: #VM putAdditionalObject: str contents. 
		
	str := String new writeStream. 
	str nextPutAll: 'Image: ';
		nextPutAll:  SystemVersion current version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']'.
	aSerializer at: #Image putAdditionalObject: str contents.
]

{ #category : #serialization }
FuelOutStackDebugAction class >> serializeTestFailureContext: aContext [

	FLDebuggerStackSerializer new serializeStackFromContext: aContext
]

{ #category : #accessing }
FuelOutStackDebugAction >> defaultLabel [

	^ 'Fuel out Stack'
]

{ #category : #accessing }
FuelOutStackDebugAction >> defaultOrder [

	^ 125
]

{ #category : #actions }
FuelOutStackDebugAction >> executeAction [

	self class serializeTestFailureContext: self interruptedContext copyStack
]

{ #category : #accessing }
FuelOutStackDebugAction >> help [
	^ 'Serialize a portion of the current stack trace using fuel.'
]

{ #category : #accessing }
FuelOutStackDebugAction >> id [

	^ #fuelOutStack
]
