Extension { #name : #CompiledMethod }

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> asActiveRingDefinition [
	"Retrieves an active RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from the compiled method"
	
	^ RGMethodDefinition new 
			name: self selector;
			parentName: self methodClass name;
			isMetaSide: self methodClass isMeta;
			asActive.

]

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> asFullRingDefinition [

	"Retrieves an active RGMethodDefinition which knows its parent <class>.
	Note that the full conversion does not happen at the level of the class. If you need that request asFullRingDefinition to the class"
	| rgClass rgMethod |
	rgClass := self realClass asRingDefinition.
	rgMethod := self asActiveRingDefinition.
	rgClass addMethod: rgMethod.
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	^ rgMethod
]

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> asHistoricalRingDefinition [

	"Retrieves a historical RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from the source file method"
	| ring |
	ring := (RGMethodDefinition named: self selector)
				parentName: self methodClass name;
				isMetaSide: self methodClass isMeta.

	self sourcePointer isZero
		ifTrue: [ "this should not happen but sometimes the system looks corrupted"
			ring protocol: self category;
				sourceCode: self sourceCode;
				stamp: self timeStamp ]
		ifFalse: [ 
			ring sourcePointer: self sourcePointer ].
	ring asHistorical.	
	
	^ ring
]

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> asPassiveRingDefinition [
	"Retrieves a passive RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from value assigned in creation"
	
	^RGMethodDefinition new 
		 	name: self selector;
			parentName: self methodClass name;
			isMetaSide: self methodClass isMeta;
			protocol: self category;
			sourceCode: self sourceCode;
			stamp: self timeStamp;
			asPassive.
]

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> asRingDefinition [
	"Retrieves an active RGMethodDefinition object based on the receiver.
	Note that its class is not converted."
	
	^ self asActiveRingDefinition
]

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> methodReference [

	| class selector |
	class := self methodClass ifNil: [^nil].
	selector := self selector ifNil: [^nil].
	^(RGMethodDefinition realClass: class selector: selector) 
		package: self package asRingDefinition; yourself
	
]

{ #category : #'*Ring-Definitions-Core' }
CompiledMethod >> realClass [
	"answer the class that I am installed in"
	
	^ self methodClass
]
