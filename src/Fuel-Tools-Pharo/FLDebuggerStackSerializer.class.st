"
Serializes a debugger stack using fuel.
"
Class {
	#name : #FLDebuggerStackSerializer,
	#superclass : #FLSerializer,
	#instVars : [
		'fileName'
	],
	#category : #'Fuel-Tools-Pharo-Debugger'
}

{ #category : #serializing }
FLDebuggerStackSerializer class >> serializeStackFromContext: aContext [
	self newDefault serializeStackFromContext: aContext
]

{ #category : #serializing }
FLDebuggerStackSerializer class >> serializeStackFromContext: aContext toFileNamed: aFileName [
	self newDefault
		serializeStackFromContext: aContext
		toFileNamed: aFileName
]

{ #category : #private }
FLDebuggerStackSerializer >> encodeDebugInformation [
	self
		at: #Timestamp putAdditionalObject: self timestampString;
		at: #VM putAdditionalObject: self vmInformationString;
		at: #Image putAdditionalObject: self imageInformationString
]

{ #category : #accessing }
FLDebuggerStackSerializer >> fileName [
	^ fileName
]

{ #category : #private }
FLDebuggerStackSerializer >> fileNameForContext: aContext [
	| date |
	date := DateAndTime now.
	^ String streamContents: [ :s | 
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

{ #category : #private }
FLDebuggerStackSerializer >> imageInformationString [
	^ String streamContents: [ :stream |
		stream	
			nextPutAll: 'Image: ';
			nextPutAll:  SystemVersion current version asString;
			nextPutAll: ' [';
			nextPutAll: Smalltalk lastUpdateString asString;
			nextPutAll: ']' ]
]

{ #category : #serializing }
FLDebuggerStackSerializer >> serializeStackFromContext: aContext [
	fileName := self fileNameForContext: aContext.
	self
		serializeStackFromContext: aContext
		toFileNamed: fileName
]

{ #category : #serializing }
FLDebuggerStackSerializer >> serializeStackFromContext: aContext toFileNamed: aFileName [
	self
		encodeDebugInformation;
		addPostMaterializationAction: [ :materialization | 
		FLDebuggerStackMaterialization new
			interruptedContext: materialization root;
			signal ];
		serialize: aContext copyStack toFileNamed: aFileName
]

{ #category : #private }
FLDebuggerStackSerializer >> timestampString [
	^ String streamContents: [ :stream |
		stream
			print: Date today;
			space;
			print: Time now ]
]

{ #category : #private }
FLDebuggerStackSerializer >> vmInformationString [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'VM: ';
			nextPutAll: Smalltalk os name asString;
			nextPutAll: ' - ';	
			nextPutAll: Smalltalk os subtype asString;
			nextPutAll: ' - ';
			nextPutAll: Smalltalk os version asString;
			nextPutAll: ' - ';
			nextPutAll: Smalltalk vm version asString ]
]
