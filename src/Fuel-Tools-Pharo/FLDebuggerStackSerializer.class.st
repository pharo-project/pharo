"
Serializes a debugger stack using fuel.
"
Class {
	#name : #FLDebuggerStackSerializer,
	#superclass : #Object,
	#category : #'Fuel-Tools-Pharo'
}

{ #category : #serializing }
FLDebuggerStackSerializer class >> encodeDebugInformationOn: aSerializer [
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
	aSerializer at: #Image putAdditionalObject: str contents
]

{ #category : #serializing }
FLDebuggerStackSerializer class >> serializeContext: aContext toFile: aFilename [

	| serializer |
	serializer := FLSerializer newDefault.
	self encodeDebugInformationOn: serializer.
	serializer addPostMaterializationAction: [ :materialization | 
		(OupsDebugRequest newForContext: materialization root)
			process: Processor activeProcess;
			label: 'External stack';
			submit ].

	serializer serialize: aContext toFileNamed: aFilename
]

{ #category : #serializing }
FLDebuggerStackSerializer >> serializeStack: aStack interruptedContext: interruptedContext [

	| date fileName |
	date := DateAndTime now.

	fileName := String streamContents: [ :s | 
		            s << 'Debugger-Stack-'.
		            s << interruptedContext receiver class instanceSide name.
		            s << '-'.
		            date printYMDOn: s.
		            s << '-'.
		            s print: date hour24.
		            s print: date minute.
		            s print: date second.
		            s << '.fuel' ].

	self class serializeContext: aStack toFile: fileName
]
