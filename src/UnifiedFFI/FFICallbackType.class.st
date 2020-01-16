"
I'm a FFICallback type. 
I can handle callback type parameters.

What I actually do is to pass the content of #thunk instVar of the objectClass as parameter. 
Thunk needs to be a callback thunk (and then an ExternalAddress).
The C side of a callback is void* (because is a pointer to a function)
"
Class {
	#name : #FFICallbackType,
	#superclass : #FFIExternalObjectType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #'emitting code' }
FFICallbackType >> basicEmitArgument: aBuilder context: aContext inCallout: aCallout [

 	self loader emitArgument: aBuilder context: aContext.

	aBuilder send: self instanceVariableName

]

{ #category : #'emitting code' }
FFICallbackType >> emitReturn: aBuilder resultTempVar: resultVar context: aContext [

	^ aBuilder 
		addTemp: #tmpResult;
		"keep invoke result into the tmpResult var"
		storeTemp: resultVar;
		popTop;
		"return := self objectClass basicNew"
		pushLiteralVariable: self objectClass binding;
		send: #basicNew;
		storeTemp: #tmpResult;
		popTop;
		"return instVarAt: (index of argName) put: result"
		pushTemp: #tmpResult;
		pushTemp: resultVar;
		send: self instanceVariableName asMutator;
		popTop;
		pushTemp: #tmpResult;
		returnTop	
]

{ #category : #accessing }
FFICallbackType >> instanceVariableName [

	^ #thunk
]

{ #category : #'emitting code' }
FFICallbackType >> offsetReadFieldAt: offsetVariableName [
	^ String streamContents: [ :stream | 
		stream << '^' << self objectClass name << ' forAddress: ((handle pointerAt: ' << offsetVariableName asString << ') asInteger)' ]
]

{ #category : #'emitting code' }
FFICallbackType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	| cr tab |
	cr := String cr.
	tab := String tab.
	^ String streamContents: [ :stream | 
		stream 
			<< 'handle ' << cr 
			<< tab << tab << 'pointerAt: ' << offsetVariableName << cr 
			<< tab << tab << 'put: (anObject ' << cr 
			<< tab << tab << tab << 'ifNotNil: [ anObject thunk asExternalAddress ]' << cr
			<< tab << tab << tab << 'ifNil: [ ExternalAddress null ])' ]
]

{ #category : #'emitting code' }
FFICallbackType >> readFieldAt: byteOffset [
	^ String streamContents: [ :stream | 
		stream << '^' << self objectClass name << ' forAddress: ((handle pointerAt: ' << byteOffset asString << ') asInteger)' ]
]

{ #category : #'emitting code' }
FFICallbackType >> writeFieldAt: byteOffset with: valueName [
	^ String streamContents: [ :stream | 
		stream << 'handle pointerAt: ' << byteOffset asString << ' put: (ExternalAddress fromAddress: anObject thunk address)' ]
]
