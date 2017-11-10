"
A FuelOutStackDebugAction is	a debugging action that serialize a portion of the current stack trace using fuel.

"
Class {
	#name : #FuelOutStackDebugAction,
	#superclass : #DebugAction,
	#category : #FuelTools-Debugger
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
FuelOutStackDebugAction class >> serializeTestFailureContext: aContext toFileNamed: aFilename [
	| serializer |
	
	serializer := FLSerializer newDefault.
	self encodeDebugInformationOn: serializer. 
	serializer addPostMaterializationAction: [ :materialization | | session |
		session := Processor activeProcess newDebugSessionNamed: 'External stack' startedAt: materialization root.
		Smalltalk tools debugger openOn: session withFullView: true].
		
	serializer
		" use the sender context, generally the current context is not interesting"
		serialize: aContext
		toFileNamed: aFilename
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

	self serializeStack
]

{ #category : #accessing }
FuelOutStackDebugAction >> id [

	^ #fuelOutStack
]

{ #category : #actions }
FuelOutStackDebugAction >> serializeStack [
	| date fileName |
	"Serialize a portion of the current stack trace using fuel.."
	date := DateAndTime now.
	
	fileName := String streamContents: [ :s|
		s 
			<< 'Debugger-Stack-';
			<< self interruptedContext receiver class theNonMetaClass name;
			<< '-'.
		date printYMDOn: s.
		s 
			<< '-'; print: date hour24; print: date minute; print: date second;
			<< '.fuel'].

	self class
		serializeTestFailureContext: self interruptedContext copyStack		
		toFileNamed: fileName
]
