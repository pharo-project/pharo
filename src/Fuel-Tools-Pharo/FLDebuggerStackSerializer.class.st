"
Serializes a debugger stack using fuel.
"
Class {
	#name : #FLDebuggerStackSerializer,
	#superclass : #Object,
	#instVars : [
		'fileName'
	],
	#category : #'Fuel-Tools-Pharo-Debugger'
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
FLDebuggerStackSerializer class >> serializeContext: aContext toFileNamed: aFilename [

	| serializer |
	serializer := FLSerializer newDefault.
	self encodeDebugInformationOn: serializer.
	serializer addPostMaterializationAction: [ :materialization | 
		FLDebuggerStackMaterialization new
			interruptedContext: materialization root;
			signal ].

	serializer serialize: aContext toFileNamed: aFilename
]

{ #category : #serializing }
FLDebuggerStackSerializer >> buildFileNameForContext: aContext [

	| date |
	date := DateAndTime now.
	fileName := String streamContents: [ :s | 
		            s << 'Debugger-Stack-'.
		            s << aContext receiver class instanceSide name.
		            s << '-'.
		            date printYMDOn: s.
		            s << '-'.
		            s print: date hour24.
		            s print: date minute.
		            s print: date second.
		            s << '.fuel' ]
]

{ #category : #accessing }
FLDebuggerStackSerializer >> fileName [

	^ fileName
]

{ #category : #serializing }
FLDebuggerStackSerializer >> serializeStackFromContext: interruptedContext [
	
	self buildFileNameForContext: interruptedContext.
	self class serializeContext: interruptedContext toFileNamed: fileName
]
