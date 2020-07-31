"
I am a breakpoint installed on one or more instance variables of a class or of an object.
"
Class {
	#name : #VariableBreakpoint,
	#superclass : #Breakpoint,
	#instVars : [
		'targetVariableNames',
		'accessStrategy',
		'isInstalled',
		'targetClassOrMethod'
	],
	#category : #'Reflectivity-VariableBreakpoint'
}

{ #category : #private }
VariableBreakpoint class >> allSlotNamesFor: aClass [
	^ aClass allSlots collect: [ :s | s name ]
]

{ #category : #'instance creation' }
VariableBreakpoint class >> newBreakpointForVariable: aVariableName forMethod: aCompiledMethod [
	^ self new
		targetClassOrMethod: aCompiledMethod;
		targetVariableNames: (OrderedCollection with: aVariableName)
]

{ #category : #'instance creation' }
VariableBreakpoint class >> newBreakpointForVariable: aVariableName inClass: aClass [
	^ self new
		targetClassOrMethod: aClass;
		targetVariableNames: (OrderedCollection with: aVariableName)
]

{ #category : #'instance creation' }
VariableBreakpoint class >> newForClass: aClass [
	^ self new
		targetClassOrMethod: aClass;
		targetVariableNames: (self allSlotNamesFor: aClass)
]

{ #category : #observers }
VariableBreakpoint class >> notifyBreakpointHit: aBreakpoint inContext: aContext node: node [
	| notification |	
	notification := BreakpointHitNotification
		on: aBreakpoint
		nodes: {node}.
	notification valueOrNil: (node variableValueInContext: aContext).
	self notifyObservers: notification
]

{ #category : #API }
VariableBreakpoint class >> watchVariable: aVariableName inClass: aClassOrCompiledMethod [
	^ (aClassOrCompiledMethod newBreakpointForVariable: aVariableName)
		install
]

{ #category : #'API - object-centric' }
VariableBreakpoint class >> watchVariable: aVariableName inObject: anObject [
	^ (self newBreakpointForVariable: aVariableName inClass: anObject class)
		scopeTo: anObject;
		install
]

{ #category : #API }
VariableBreakpoint class >> watchVariableReads: aVariableName inClass: aClassOrCompiledMethod [
	^ (aClassOrCompiledMethod newBreakpointForVariable: aVariableName)
		accessStrategy: #read;
		install
]

{ #category : #'API - object-centric' }
VariableBreakpoint class >> watchVariableReads: aVariableName inObject: anObject [
	^ (self newBreakpointForVariable: aVariableName inClass: anObject class)
		accessStrategy: #read;
		scopeTo: anObject;
		install
]

{ #category : #API }
VariableBreakpoint class >> watchVariableWrites: aVariableName inClass: aClassOrCompiledMethod [
	^ (aClassOrCompiledMethod newBreakpointForVariable: aVariableName)
		accessStrategy: #write;
		install
]

{ #category : #'API - object-centric' }
VariableBreakpoint class >> watchVariableWrites: aVariableName inObject: anObject [
	^ (self newBreakpointForVariable: aVariableName inClass: anObject class)
		accessStrategy: #write;
		scopeTo: anObject;
		install
]

{ #category : #API }
VariableBreakpoint class >> watchVariablesInClass: aClass [
	^ (self newForClass: aClass) install
]

{ #category : #'API - object-centric' }
VariableBreakpoint class >> watchVariablesInObject: anObject [
	^ (self newForClass: anObject class)
		scopeTo: anObject;
		install
]

{ #category : #API }
VariableBreakpoint class >> watchVariablesReadsInClass: aClass [
	^ (self newForClass: aClass)
		accessStrategy: #read;
		install
]

{ #category : #'API - object-centric' }
VariableBreakpoint class >> watchVariablesReadsInObject: anObject [
	^ (self newForClass: anObject class)
		accessStrategy: #read;
		scopeTo: anObject;
		install
]

{ #category : #API }
VariableBreakpoint class >> watchVariablesWritesInClass: aClass [
	^ (self newForClass: aClass)
		accessStrategy: #write;
		install
]

{ #category : #'API - object-centric' }
VariableBreakpoint class >> watchVariablesWritesInObject: anObject [
	^ (self newForClass: anObject class)
		accessStrategy: #write;
		scopeTo: anObject;
		install
]

{ #category : #accessing }
VariableBreakpoint >> accessStrategy [
	^ accessStrategy
]

{ #category : #accessing }
VariableBreakpoint >> accessStrategy: anObject [
	accessStrategy := anObject
]

{ #category : #initialize }
VariableBreakpoint >> initialize [
	super initialize.
	accessStrategy := #all.
	targetVariableNames := OrderedCollection new.
	isInstalled := false
]

{ #category : #install }
VariableBreakpoint >> install [
	self isInstalled
		ifTrue: [ ^ self ].
	targetVariableNames
		do: [ :vName | 
			targetClassOrMethod
				link: self link
				toVariableNamed: vName
				option: self accessStrategy ].
	isInstalled := true.
	self class addBreakpoint: self.
	self setAsNodeProperty 
]

{ #category : #testing }
VariableBreakpoint >> isInstalled [
	^isInstalled
]

{ #category : #testing }
VariableBreakpoint >> isVariableBreakpoint [
	^true
]

{ #category : #install }
VariableBreakpoint >> link [
	self flag: 'This is not clear if it should break after or before a variable has been accessed/written'.
	^ super link
		control: #after;
		yourself
]

{ #category : #install }
VariableBreakpoint >> remove [
	super remove.
	targetInstance := nil.
	isInstalled := false
]

{ #category : #install }
VariableBreakpoint >> removeFromNodeProperty [
	 self link nodes do: [ :n | n removeBreakpoint: self ]
]

{ #category : #api }
VariableBreakpoint >> scopeTo: anInstance [
	super scopeTo: anInstance.
	self condition: [ :ctx | ctx receiver == targetInstance ]
]

{ #category : #install }
VariableBreakpoint >> setAsNodeProperty [
	 self link nodes do: [ :n | n addBreakpoint: self ]
]

{ #category : #accessing }
VariableBreakpoint >> targetClassOrMethod [
	^ targetClassOrMethod
]

{ #category : #accessing }
VariableBreakpoint >> targetClassOrMethod: anObject [
	targetClassOrMethod := anObject
]

{ #category : #accessing }
VariableBreakpoint >> targetVariableNames [
	^ targetVariableNames
]

{ #category : #accessing }
VariableBreakpoint >> targetVariableNames: anObject [
	targetVariableNames := anObject
]
