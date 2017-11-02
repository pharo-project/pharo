Class {
	#name : #HEInstallWholePackageTest,
	#superclass : #HEAbstractBinaryClassBuilderTest,
	#instVars : [
		'builder',
		'newClass'
	],
	#category : 'Hermes-Tests'
}

{ #category : #utils }
HEInstallWholePackageTest >> changeClassesNames: aPackage [
	aPackage instVarNamed: #packageName put: #'Hermes-Tests-External-Package2'.
	aPackage classes
		do: [ :e | 
			e instVarNamed: #category put: #'Hermes-Tests-External-Package2'.
			e instVarNamed: #className put: (e className , '2') asSymbol.
			self updateMethodLiterals: e methods.
			self updateMethodLiterals: e classSideMethods ].

	aPackage traits
		do: [ :e | 
			e instVarNamed: #category put: #'Hermes-Tests-External-Package2'.
			e instVarNamed: #traitName put: (e className , '2') asSymbol.
			self updateMethodLiterals: e methods.
			self updateMethodLiterals: e classSideMethods ].

	aPackage extensionMethods
		do: [ :e | 
			e instVarNamed: #name put: (e name , '2') asSymbol.
			e instVarNamed: #protocol put: #'*Hermes-Tests-External-Package2' ].

]

{ #category : #initialization }
HEInstallWholePackageTest >> setUp [
	super setUp.
	builder := HEInstaller new
]

{ #category : #initialization }
HEInstallWholePackageTest >> tearDown [
	| aPackage |
	aPackage := RPackageOrganizer default packageNamed:#'Hermes-Tests-External-Package2' ifAbsent: nil.
	aPackage ifNotNil: #removeFromSystem.
	super tearDown.
]

{ #category : #tests }
HEInstallWholePackageTest >> testExportingWholePackageAndImportingIt [
	| hePackage readPackage newPackage oldPackage obj obj2 |
	oldPackage := RPackageOrganizer default packageNamed: #'Hermes-Tests-External-Package'.
	hePackage := HEPackage for: oldPackage.
	hePackage writeInto: writer.
	writer flush.

	readPackage := HEPackage readFrom: reader.
	self changeClassesNames: readPackage.

	builder installPackage: readPackage.
	newPackage := RPackageOrganizer default packageNamed: #'Hermes-Tests-External-Package2'.

	self assert: newPackage classes size equals: oldPackage classes size.

	newClass := Smalltalk at: #AClassToBeExtractedFactory2.
	obj := newClass new.
	obj2 := obj anObject.

	self assert: obj2 class equals: (Smalltalk at: #AClassToBeExtracted2)
]

{ #category : #utils }
HEInstallWholePackageTest >> updateMethodLiterals: methods [
	methods
		do: [ :e | 
			e instVarNamed: #className put: (e className , '2') asSymbol.
			e literals
				do: [ :lit | 
					(lit isKindOf: HEGlobalVariable)
						ifTrue: [ lit instVarNamed: #value put: (lit value , '2') asSymbol ] ] ]
]
