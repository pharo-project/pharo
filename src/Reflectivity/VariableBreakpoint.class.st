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
	#category : #'Reflectivity-Breakpoints'
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
	| announcement |	
	announcement := BreakpointHit
		on: aBreakpoint
		nodes: {node}.
	announcement valueOrNil: (node variableValueInContext: aContext).
	SystemAnnouncer uniqueInstance announce: announcement
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

{ #category : #install }
VariableBreakpoint >> breakLink [

	^ super breakLink
		  control: #after;
		  yourself
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
	self setAsNodeProperty.
	self class addBreakpoint: self.
]

{ #category : #testing }
VariableBreakpoint >> isInstalled [
	^isInstalled
]

{ #category : #testing }
VariableBreakpoint >> isVariableBreakpoint [
	^true
]

{ #category : #removing }
VariableBreakpoint >> remove [
	super remove.
	targetInstance := nil.
	isInstalled := false
]

{ #category : #removing }
VariableBreakpoint >> removeFromClass: aClass [

	"If we're targeting a temp var in a method,
	then we got here because we detected that method belong to the class
	passed as parameter. We need to uninstall."
	self targetClassOrMethod isCompiledMethod ifTrue: [ 
		self remove.
		^ self ].

	"If the removed class is the class we target or one of its superclasses,
	then after that class is removed we have no reason to exist. We need to uninstall."
	(self targetClassOrMethod = aClass or: [ 
		 self targetClassOrMethod allSuperclasses includes: aClass ]) 
		ifTrue: [ 
			self remove.
			^ self ].

	"We removed a aClass, but it is not our target class nor one of its superclasses.
	It is then one of its subclasses, we can continue to exist since the variable
	we target still exist in the target class.
	However, we need to remove the nodes of the removed class from our metalink"
	(self link nodes select: [ :n | n methodNode methodClass == aClass ]) 
		do: [ :n | self link removeNode: n ]
]

{ #category : #removing }
VariableBreakpoint >> removeFromMethod: aMethod [
	(self link nodes select: [ :n | n methodNode method == aMethod ]) 
		do: [ :n | self link removeNode: n ]
]

{ #category : #removing }
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
