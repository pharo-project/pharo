Extension { #name : #Class }

{ #category : #'*Reflectivity' }
Class >> intanceSpecificMetaLinksAvailable [
	^false
]

{ #category : #'*Reflectivity' }
Class >> isInClassHierarchyOf: aClass [

	^ (self includesBehavior: aClass) or: [ 
		  aClass includesBehavior: self ]
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toAST: aNode [
	aNode link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toClassVariable: aClassVariable [
	aClassVariable link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toClassVariableNamed: aClassVariableName [
	self
		link: aMetaLink
		toClassVariable: (self nonAnonymousClass classVariableNamed: aClassVariableName asSymbol)
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toSlot: aSlot [
	aSlot link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toSlotNamed: aSlotName [
	self link: aMetaLink toSlot: (self nonAnonymousClass slotNamed: aSlotName)
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toTemporary: aTempVar [
	self flag: 'Not sure what it should do. The tests are not passing for this... See method testLinkClassToTempVarNamed'.
	aTempVar link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toVariableNamed: vName option: accessStrategy [
	^aMetaLink
		installOnVariable: (self lookupVar: vName)
		for: self
		option: accessStrategy
		instanceSpecific: false
]

{ #category : #'*Reflectivity' }
Class >> newBreakpointForVariable: aVariableName [
	^ VariableBreakpoint
		newBreakpointForVariable: aVariableName
		inClass: self
]

{ #category : #'*Reflectivity' }
Class >> nonAnonymousClass [
	^ self
]
