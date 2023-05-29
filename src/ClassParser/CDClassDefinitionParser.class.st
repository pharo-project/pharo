"
I use the default node representation of classes to parse class definition and  return a dedicated class tree.

The basic idea behing the implementation is to reuse as much as possible as the current parser infrastructure. This is why this class defines a visitMessageNode: method. 

Main public API is 
	- parse: aString
"
Class {
	#name : #CDClassDefinitionParser,
	#superclass : #CDAbstractClassDefinitionParser,
	#category : #'ClassParser-Parser'
}

{ #category : #parsing }
CDClassDefinitionParser >> handleClassName: aNode withType: aSymbol [
	| layoutClass |
	self handleClassName: aNode.
	layoutClass := ObjectLayout layoutForSubclassDefiningSymbol: aSymbol.

	"if we get back CompiledMethodLayout or ByteLayout, we need to check the class name"
	(layoutClass = CompiledMethodLayout or: [ layoutClass = ByteLayout ]) ifTrue: [
		layoutClass := (classDefinition className = 'CompiledMethod')
					ifTrue:  [  CompiledMethodLayout ]
					ifFalse: [  ByteLayout ]].
	classDefinition layoutClass: (layoutClass)
]

{ #category : #parsing }
CDClassDefinitionParser >> handleSharedPoolsFromNode: aNode [
	| shareds sharedPoolNodes sharedPoolString |
	sharedPoolString := aNode value.
	shareds := sharedPoolString substrings.
	sharedPoolNodes := shareds
		collect: [ :slotName |
			| start |
			start := aNode start + (sharedPoolString findString: slotName).
			self sharedPoolNodeClass
				node: aNode
				name: slotName
				start: start
				stop: start + slotName size ].
	classDefinition sharedPools: sharedPoolNodes
]

{ #category : #parsing }
CDClassDefinitionParser >> handleSuperclassNode: aSuperclassNode [
	| aSuperclassName superclassNode |
	aSuperclassName := (aSuperclassNode isLiteralNode
		ifTrue: [ aSuperclassNode value asString ]
		ifFalse: [ aSuperclassNode name ]) asSymbol.
	superclassNode := self classNameNodeClass new
		originalNode: aSuperclassNode;
		className: aSuperclassName.
	classDefinition
		superclassName: aSuperclassName
		astNode: superclassNode
]

{ #category : #parsing }
CDClassDefinitionParser >> handleTraitDefinitionFromNode: aNode [

	| traitComposition |
	traitComposition := CDTraitCompositionBuilder new buildFrom: aNode.
	classDefinition traitDefinition: traitComposition
]

{ #category : #testing }
CDClassDefinitionParser >> isInstanceSideDefinition: aRBMessageNode [
	"Based on the Point or Point class structure, returns whether the definition is a class or instance side definition."
	^ aRBMessageNode receiver isMessage not
]

{ #category : #parsing }
CDClassDefinitionParser >> parseRootNode: expressionTree [
	self setAdequateClassDefinitionFrom: expressionTree.
	classDefinition originalNode: expressionTree.
	expressionTree acceptVisitor: self.
	^ classDefinition
]

{ #category : #parsing }
CDClassDefinitionParser >> parseSelectorPart: aString withArgument: aNode [

	"We could do this with reflection, or with a dictionary and closures.
	I chose to use a series of if for readability only."

	(ObjectLayout allSubclassDefiningSymbols includes: aString)
		ifTrue: [ ^ self handleClassName: aNode withType: aString  ].
	aString =	 #instanceVariableNames:
		ifTrue: [ ^ self handleInstanceVariablesFromNode: aNode ].
	aString =	 #classVariableNames:
		ifTrue: [ ^ self handleSharedVariableNames: aNode ].
	aString =	 #category:
		ifTrue: [ ^ self handlePackage: aNode ].
	aString =	 #package:
		ifTrue: [ ^ self handlePackage: aNode ].
	aString =	 #uses:
		ifTrue: [ ^ self handleTraitDefinitionFromNode: aNode ].
	aString =	 #poolDictionaries:
		ifTrue: [ ^ self handleSharedPoolsFromNode: aNode ].

	CDUnrecognizedClassPart new
		classDefinition: self;
		unrecognizedPart: aString;
		signal
]

{ #category : #internal }
CDClassDefinitionParser >> setAdequateClassDefinitionFrom: expressionTree [
	"set a class or metaclass class definition as class definition object."
	(self isInstanceSideDefinition: expressionTree)
		ifTrue: [ self beClassDefinition ]
		ifFalse: [ self beMetaclassDefinition ]
]

{ #category : #parsing }
CDClassDefinitionParser >> visitMessageNode: aRBMessageNode [

	(self isInstanceSideDefinition: aRBMessageNode)
		ifTrue: [ self handleSuperclassNode: aRBMessageNode receiver ]
		ifFalse: [ self handleClassName: aRBMessageNode receiver receiver ].

	aRBMessageNode selectorParts
		with: aRBMessageNode arguments
		do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ]
]
