"
I'm an abstract class to implement FFI external types (types who will later be mapped to something understandable for a C library)
"
Class {
	#name : #FFIExternalType,
	#superclass : #Object,
	#instVars : [
		'pointerArity',
		'loader'
	],
	#category : #'UnifiedFFI-Types'
}

{ #category : #converting }
FFIExternalType class >> asExternalTypeOn: generator [
	^ self new
]

{ #category : #accessing }
FFIExternalType class >> externalType [
	^ self subclassResponsibility
]

{ #category : #accessing }
FFIExternalType class >> externalTypeAlignment [
	"Answer a number of bytes, which takes a value of given type
	(not a pointer to it)"
	self subclassResponsibility 	
]

{ #category : #accessing }
FFIExternalType class >> externalTypeSize [
	"Answer a number of bytes, which takes a value of given type
	(not a pointer to it)"
	self subclassResponsibility 	
]

{ #category : #private }
FFIExternalType class >> naturalPointerArity [
	"Indicates 'natural' pointer artity of type (atomic types are zero while any kind of 
	 pointer/reference/etc. is one)"
	^ 0
]

{ #category : #'instance creation' }
FFIExternalType class >> newBuffer [
	"This method is used to provide convencience buffer accessor to types. 
	 This is useful when you need to pass a reference to an argument to take the value later. 
	 For example: 
	
  void getDimensions(double *width, double *height) {
    *width = 2.0;
    *height = 3.0;
  }
  int main() {
    double w, h;
    get_dimensions( &w, &h);
    printf(""%f %f"", w, h)
  }
==> 2.0 3.0

would have an FFI declaration...

  ffi_getDimensions__width: width height: height
    ^ #( (void getDimensions( double *width, double *height) )

  testMain
        | widthBuffer heightBuffer width height |
        widthBuffer := FFIFloat64 newBuffer.
        heightBuffer := FFIFloat64 newBuffer.
        ffi_getDimensions__width: widthBuffer height: heightBuffer.
        width := widthBuffer doubleAt: 1.
        height := heightBuffer doubleAt: 1.
        Transcript crShow: width; tab; show: height.
==> 2.0 3.0
	"
	^ ByteArray new: self externalTypeSize
]

{ #category : #accessing }
FFIExternalType class >> pointerAlignment [
	^ OSPlatform current ffiPointerAlignment
]

{ #category : #accessing }
FFIExternalType class >> pointerSize [
	"Answer a number of bytes, which takes a pointer value"
	^ Smalltalk vm wordSize
]

{ #category : #public }
FFIExternalType class >> resolveType: aTypeName [
	^ FFICallout new resolveType: aTypeName
	
]

{ #category : #public }
FFIExternalType class >> sizeOf: aTypeName [
	^ (self resolveType: aTypeName) typeSize
]

{ #category : #'emitting code' }
FFIExternalType >> basicEmitArgument: aBuilder context: aContext inCallout: aCallout [
	self loader
		emitArgument: aBuilder 
		context: aContext
]

{ #category : #private }
FFIExternalType >> basicHandle: aHandle at: index [
	self subclassResponsibility
]

{ #category : #private }
FFIExternalType >> basicHandle: aHandle at: index put: value [
	self subclassResponsibility
]

{ #category : #callbacks }
FFIExternalType >> callbackReturnOn: callbackContext for: anObject [
	"By default, I answer an integral return (not a float)"
	^ callbackContext wordResult: anObject
]

{ #category : #callbacks }
FFIExternalType >> callbackValueFor: anObject at: index [
	"This is the value for a callback. 
	 The callback parameters came from an external adress who can be treated as a ByteArray, so it 
	 works the same as an FFIExternalArray (at least for now)"
	^ self handle: anObject at: index
]

{ #category : #accessing }
FFIExternalType >> defaultReturnOnError [
	"In case of a callback error, the image will try to show a debugger and that will most ot the 
	 time crashes the VM (because it will break the process and will let a C function waiting and 
	 and in incorrect state). 
	 To prevent that, we use #on:fork:return: (look for senders) and, while forking the error to 
	 allow user to debug his error, we also return a 'default' value, that may be also wrong."

	^ self subclassResponsibility
]

{ #category : #'emitting code' }
FFIExternalType >> emitArgument: aBuilder context: aContext inCallout: aCallout [
	self basicEmitArgument: aBuilder context: aContext inCallout: aCallout.
	self needsArityPacking 
		ifTrue: [ self  emitPointerArityRoll: aBuilder context: aContext ]
]

{ #category : #'stack parameter classification' }
FFIExternalType >> emitFlatStructureLayoutFieldInto: flatStructureLayout [
	flatStructureLayout addField: self stackParameterClass size: self typeSize alignment: self typeAlignment
]

{ #category : #'emitting code' }
FFIExternalType >> emitPointerArityRoll: aBuilder context: aContext [
	self loader 
		emitPointerArityPack: aBuilder 
		context: aContext 
		arity: self pointerArity
]

{ #category : #'emitting code' }
FFIExternalType >> emitReturn: aBuilder resultTempVar: resultVar context: aContext [ 
	^ aBuilder returnTop	
]

{ #category : #'emitting code' }
FFIExternalType >> emitReturn: aBuilder resultTempVar: resultVar context: aContext inCallout: aCallout [
	
	^ self emitReturn: aBuilder resultTempVar: resultVar context: aContext 
]

{ #category : #'emitting code' }
FFIExternalType >> emitReturnArgument: builder context: aContext [
	"Some times functions need some post-process (for example, to unpack pointers). 
	 This call MUST exit with result value in top of the stack (otherwise it will 
	interfere with emitReturn:resultTempVar:context:"
	self loader 
		emitPointerArityUnpack: builder 
		type: self
		context: aContext
]

{ #category : #accessing }
FFIExternalType >> externalType [
	^ self class externalType
]

{ #category : #accessing }
FFIExternalType >> externalTypeAlignment [
	^ self class externalTypeAlignment
]

{ #category : #accessing }
FFIExternalType >> externalTypeSize [
	^ self class externalTypeSize
]

{ #category : #accessing }
FFIExternalType >> externalTypeWithArity [
	^ self pointerArity > 0 
		ifTrue: [ self externalType asPointerType ]
		ifFalse: [ self externalType ]
]

{ #category : #'accessing array' }
FFIExternalType >> handle: aHandle at: index [ 
	self isPointer ifTrue: [ ^ aHandle pointerAt: index ].
	^ self basicHandle: aHandle at: index
]

{ #category : #'accessing array' }
FFIExternalType >> handle: aHandle at: index put: value [
	self isPointer ifTrue: [ ^ aHandle pointerAt: index put: value ].
	^ self basicHandle: aHandle at: index put: value
]

{ #category : #initialization }
FFIExternalType >> initialize [
	super initialize.
	pointerArity := 0
]

{ #category : #testing }
FFIExternalType >> isExternalStructure [

	^ false
]

{ #category : #testing }
FFIExternalType >> isExternalType [
	^ true
]

{ #category : #testing }
FFIExternalType >> isFloatType [

	^ false
]

{ #category : #testing }
FFIExternalType >> isPointer [
	^ self pointerArity > 0
]

{ #category : #testing }
FFIExternalType >> isVoid [
	^ false
]

{ #category : #accessing }
FFIExternalType >> loader [
	^ loader
]

{ #category : #accessing }
FFIExternalType >> loader: aLoader [
	loader := aLoader
]

{ #category : #testing }
FFIExternalType >> needsArityPacking [
	"Regular types needs to be ''rolled'' if they are passed as pointers to its calling functions. 
	 For example, executing consecutivelly this (simplified) two functions: 
	 [[[
	 	time := self ffiCall: #(time_t time(time_t* t) ). ""this will answer a long.""
	 	self ffiCall: #(tm* localtime(time_t* time) ) ""this requires a pointer to time."" 
	 ]]]
	 This mechanism allows UnifiedFFI to perform the roll of this pointers for you (it performs 
	 the equivallent of ==&time== in C). 
	
	 For packing/unpacking logic, arity needs to be bigger than inherent type arity. 
	 Means that if I have a type that is naturally a pointer (for example an ExternalAddress, who 
	 is a 'void*'), it will have a natural arity of 1, then I pack if arity is bigger. 
	 Other cases could need to be rolled when pointer arity is diffrent."
	^ self pointerArity > self class naturalPointerArity
]

{ #category : #testing }
FFIExternalType >> needsArityUnpacking [
	"Simple types do not need ''unpacking'' because they can not used as buffers (to receive values 
	 from C functions).
	 For instance, in case you have functions with the form:
	 [[[ 
		self ffiCall: #(void getPoint( double *x, double *y)) 
	 ]]]
	 you cannot use instances of Float (since they are immutable in Pharo)... in that case you will
	 need to use an FFIExternalValueHolder."
	^ false
]

{ #category : #'instance creation' }
FFIExternalType >> newBuffer [
	"as its counterpart on class-side (newBuffer), this method get a memory segment that can be translated to C function as reference (to take later it's value). 
	For example:  

  ffi_getDimensions__width: width height: height
    ^ #( (void getDimensions( double *width, double *height) )

  testMain
        | widthBuffer heightBuffer width height |
        widthBuffer := (FFIExternalType resolveType: #double) newBuffer.
        heightBuffer := (FFIExternalType resolveType: #double) newBuffer.
        ffi_getDimensions__width: widthBuffer height: heightBuffer.
        width := widthBuffer doubleAt: 1.
        height := heightBuffer doubleAt: 1.
        Transcript crShow: width; tab; show: height.
==> 2.0 3.0
	"
	^ self class newBuffer
]

{ #category : #'emitting code' }
FFIExternalType >> offsetReadFieldAt: offsetVariableName [
	^ self externalTypeWithArity offsetReadFieldAt: offsetVariableName
]

{ #category : #'emitting code' }
FFIExternalType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	^ self externalTypeWithArity 
		offsetWriteFieldAt: offsetVariableName 
		with: valueName
]

{ #category : #accessing }
FFIExternalType >> pointerAlignment [
	^ self class pointerAlignment
]

{ #category : #accessing }
FFIExternalType >> pointerArity [
	^ pointerArity
]

{ #category : #accessing }
FFIExternalType >> pointerArity: additionalArity [
	pointerArity := pointerArity + additionalArity.
]

{ #category : #accessing }
FFIExternalType >> pointerSize [
	^ self class pointerSize
]

{ #category : #printing }
FFIExternalType >> printOn: aStream [
	super printOn: aStream.
	pointerArity timesRepeat: [ aStream nextPut: $* ]
]

{ #category : #'emitting code' }
FFIExternalType >> readFieldAt: byteOffset [
	^ self externalTypeWithArity readFieldAt: byteOffset
]

{ #category : #'stack parameter classification' }
FFIExternalType >> stackParameterClass [
	^ self isPointer ifTrue: [ #integer ] ifFalse: [ self stackValueParameterClass ]
]

{ #category : #'stack parameter classification' }
FFIExternalType >> stackValueParameterClass [
	self subclassResponsibility
]

{ #category : #accessing }
FFIExternalType >> typeAlignment [
	"Answer a number of bytes, which receiver type takes in memory"
	self pointerArity > 0 ifTrue: [ ^ self pointerAlignment ].
	^ self externalTypeAlignment
]

{ #category : #accessing }
FFIExternalType >> typeSize [
	"Answer a number of bytes, which receiver type takes in memory"
	self pointerArity > 0 ifTrue: [ ^ self pointerSize ].
	^ self externalTypeSize
]

{ #category : #testing }
FFIExternalType >> validateAfterParse: typeAndArityTuple [
	"After parse an argument or return, some times I need to validate arity is correct. 
	 This usually is ok, but since UFFI has types who do not have sense if they are not referenced as 
	 pointer (check my overrides), I act as an ''after parse'' validation."

]

{ #category : #'emitting code' }
FFIExternalType >> writeFieldAt: byteOffset with: valueName [
	^ self externalTypeWithArity 
		writeFieldAt: byteOffset 
		with: valueName
]
