Class {
	#name : #RBNamespace2,
	#superclass : #RBNamespace,
	#category : #'Refactoring2-Core-Model'
}

{ #category : #transforming }
RBNamespace2 >> addProtocolNamed: aString in: aClass [

	^ changes addProtocolNamed: aString in: aClass
]

{ #category : #transforming }
RBNamespace2 >> category: aString for: aClass [

	^ changes addChange: (RBClassCategoryChange category: aString for: aClass)
]

{ #category : #converting }
RBNamespace2 >> classObjectFor: anObject [

	(anObject isBehavior or: [anObject isTrait])
		ifTrue: [ ^ self classFor: anObject ].
	anObject isSymbol
		ifTrue: [ ^ self classNamed: anObject ].
	^ anObject
]

{ #category : #transforming }
RBNamespace2 >> comment: aString in: aClass [

	self deprecated: 'use RBClass2 instead'
]

{ #category : #transforming }
RBNamespace2 >> convertClasses: classSet select: aBlock using: searchReplacer [
 
	classSet do: [ :aClass | 
		(aBlock value: aClass) do: [ :selector |
			aClass convertMethod: selector using: searchReplacer ]]
]

{ #category : #converting }
RBNamespace2 >> createNewClassFor: aBehavior [
	| nonMeta meta className |
	className := aBehavior instanceSide name.
	nonMeta := (self rbClassClass existingNamed: className)
		model: self;
		yourself.
	meta := (self rbMetaclassClass existingNamed: className)
		model: self;
		yourself.
	^ changedClasses at: className put: (Array with: nonMeta with: meta)
]

{ #category : #converting }
RBNamespace2 >> defineClass: aString [
	| change newClass newClassName |
	change := changes defineClass: aString.
	newClassName := change changeClassName.
	newClass := self classNamed: newClassName.
	newClass
		ifNil: [ | newMetaclass |
			self unmarkAsRemoved: ' class'.
			newClass := self rbClassClass named: newClassName.
			newMetaclass := self rbMetaclassClass named: newClassName.
			newClass model: self.
			newMetaclass model: self.
			newClasses
				at: newClassName
				put: (Array with: newClass with: newMetaclass) ].
	newClass superclass: (self classNamed: change superclassName).
	newClass superclass
		ifNil: [ self rootClasses add: newClass.
			newClass theMetaClass
				superclass: (self classFor: Object class superclass) ]
		ifNotNil: [ newClass theMetaClass superclass: newClass superclass theMetaClass ].
	newClass instanceVariableNames: change instanceVariableNames.
	newClass classVariableNames: change classVariableNames.
	newClass poolDictionaryNames: change sharedPoolNames.
	newClass category: change category.
	^ change
]

{ #category : #converting }
RBNamespace2 >> rbClassClass [
	^ RBClass2
]

{ #category : #converting }
RBNamespace2 >> rbMetaclassClass [
	^ RBMetaclass2
]

{ #category : #transforming }
RBNamespace2 >> removeProtocolNamed: aString in: aClass [

	^ changes removeProtocolNamed: aString in: aClass
]

{ #category : #transforming }
RBNamespace2 >> renameClassVariable: oldName to: newName in: aRBClass around: aBlock [

	self deprecated: 'Use RBClass2 instead'
]

{ #category : #transforming }
RBNamespace2 >> renameInstanceVariable: oldName to: newName in: aRBClass around: aBlock [

	self deprecated: 'Use RBAbstractClass2 instead'
]

{ #category : #transforming }
RBNamespace2 >> selector: aSelector in: aClass classified: aProtocol [

	^ changes
		selector: aSelector
		in: aClass
		classified: aProtocol
]
