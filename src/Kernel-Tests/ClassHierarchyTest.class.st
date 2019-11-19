"
SUnit tests for the class hierarchy
"
Class {
	#name : #ClassHierarchyTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #fixing }
ClassHierarchyTest class >> fixSlotScope [
	"postscript for issue 11596"
	
	| ivName all candidates toRebuild |
    ivName := 'anIVNameImPrettySureNobodyUses'.
    all := Smalltalk allClasses flatCollect: [ :e | { e . e class } ].
    candidates := all reject: [ :e | e superclass isNil or: [e classLayout slotScope isKindOf: LayoutEmptyScope ] ].
    toRebuild := candidates reject: [ :e | e superclass classLayout slotScope == e classLayout slotScope parentScope ].
    toRebuild sort: [ :a :b | a allSuperclasses size > b allSuperclasses size ].
    toRebuild do: [ :e | 
    e addInstVarNamed: ivName.
        (e isClassSide
          ifTrue: [ (Smalltalk at: e instanceSide name) classSide ]
          ifFalse: [ Smalltalk at: e name ]) removeInstVarNamed: ivName ]
]

{ #category : #fixing }
ClassHierarchyTest class >> fixSubclasses [
	"Fix all the missing subclasses"
	"ClassHierarchyTest fixSubclasses "
	SystemNavigation new allClassesDo: [ :cls|
		(cls superclass subclasses includes: cls)
			ifFalse: [ cls superclass addSubclass: cls ]]
]

{ #category : #tests }
ClassHierarchyTest >> testObjectFormatInstSize [
	| classes offending |
	"check that #instSize is in sync with #allInstVarNames"
	offending := SystemNavigation default allBehaviors reject: [ :cls | cls instSize = (cls allSlots reject: #isVirtual) size ].
	self assertCollection: offending hasSameElements: {}.

	"instSpec is only 0 for classes without any inst vars or variable fields"
	classes := SystemNavigation default allBehaviors select: [ :cls | cls isTrait not and: [ cls instSpec = 0 ] ].
	offending := classes reject: [ :each | each instVarNames isEmpty or: [ each isVariable not ] ].
	self assertCollection: offending hasSameElements: {}.

]

{ #category : #tests }
ClassHierarchyTest >> testSubclassInstVar [
	| subclasses |
	SystemNavigation new
		allClassesDo: [ :cls | 
			subclasses := cls subclasses.
			self assert: subclasses isNil not.
			subclasses
				do: [ :subclass | 
					self assert: (subclasses occurrencesOf: subclass) equals: 1.
					self assert: subclass superclass identicalTo: cls ] ]
]

{ #category : #tests }
ClassHierarchyTest >> testSubclasses [
	" self class fixSubclasses "

	SystemNavigation new
		allClassesDo: [ :cls | 
			self
				assert: (cls superclass isNil or: [ cls superclass subclasses includes: cls ])
				description: cls name , ' is not in ' , (cls superclass ifNotNil: [ :superclass | superclass name ] ifNil: [ 'nil' ]) , '''s subclasses' ]
]
