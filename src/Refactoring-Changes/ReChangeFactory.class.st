"
This object is the  abstract factory of all the changes, 
the idea is to have another version of the changes that can use the atomic class installer.
"
Class {
	#name : 'ReChangeFactory',
	#superclass : 'Object',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'accessing' }
ReChangeFactory class >> instance [

	^ ReChangeManager instance changeFactory
]

{ #category : 'refactory changes - classes' }
ReChangeFactory >> addClassDefinition: definition [

	^ ReAddClassChange make: definition
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> addClassVariable: variableName to: aClass [

	^ ReAddClassVariableChange add: variableName to: aClass
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> addInstanceVariable: variableName to: aClass [

	^ ReAddInstanceVariableChange add: variableName to: aClass
]

{ #category : 'refactory changes - methods' }
ReChangeFactory >> addMethodSource: sourceCode in: aClass [

	^ ReAddMethodChange compile: sourceCode in: aClass
]

{ #category : 'refactory changes - methods' }
ReChangeFactory >> addMethodSource: sourceCode in: aClass classified: aProtocol [

	^ ReAddMethodChange
		  compile: sourceCode
		  in: aClass
		  classified: aProtocol
]

{ #category : 'refactory changes - packages' }
ReChangeFactory >> addPackageNamed: aString [

	^ ReAddPackageChange addPackageNamed: aString
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> addPoolVariable: variableName to: aClass [

	^ ReAddPoolVariableChange add: variableName to: aClass
]

{ #category : 'refactory changes - classes' }
ReChangeFactory >> comment: aComment in: aRBClass [

	^ ReCommentChange comment: aComment in: aRBClass
]

{ #category : 'refactory changes - composites' }
ReChangeFactory >> compositeRefactoryChange [

	^ ReCompositeChange new
]

{ #category : 'refactory changes - composites' }
ReChangeFactory >> compositeRefactoryChangeNamed: name [

	^ ReCompositeChange named: name
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> removeClassNamed: aClassName [

	^ ReRemoveClassChange removeClassName: aClassName
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> removeClassVariable: variableName from: aClass [

	^ ReRemoveClassVariableChange remove: variableName from: aClass
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> removeInstanceVariable: variableName from: aClass [

	^ ReRemoveInstanceVariableChange remove: variableName from: aClass
]

{ #category : 'refactory changes - methods' }
ReChangeFactory >> removeMethod: aSelector from: aClass [

	^ ReRemoveMethodChange remove: aSelector from: aClass
]

{ #category : 'refactory changes - packages' }
ReChangeFactory >> removePackageNamed: aString [

	^ ReRemovePackageChange removePackageNamed: aString
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> removePoolVariable: variableName from: aClass [

	^ ReRemovePoolVariableChange remove: variableName from: aClass
]

{ #category : 'refactory changes - classes' }
ReChangeFactory >> renameClass: aRBClass to: newName [

	^ ReRenameClassChange rename: aRBClass name to: newName
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> renameClassVariable: oldName to: newName in: aClass [

	^ ReRenameClassVariableChange rename: oldName to: newName in: aClass
]

{ #category : 'refactory changes - variables' }
ReChangeFactory >> renameInstanceVariable: oldName to: newName in: aClass [

	^ ReRenameInstanceVariableChange rename: oldName to: newName in: aClass
]

{ #category : 'refactory changes - packages' }
ReChangeFactory >> renamePackage: aRBPackage to: newName [

	^ ReRenamePackageChange rename: aRBPackage name to: newName
]

{ #category : 'refactory changes - packages' }
ReChangeFactory >> renamePackageNamed: aRBPackage to: newName [

	^ ReRenamePackageChange rename: aRBPackage to: newName
]
