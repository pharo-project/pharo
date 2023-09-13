"
A writer for Smalltalk code
"
Class {
	#name : #MCFileTreeStWriter,
	#superclass : #MCFileTreeAbstractStWriter,
	#category : #'MonticelloFileTree-Core'
}

{ #category : #visiting }
MCFileTreeStWriter >> visitClassDefinition: definition [
    orderedClassNames add: definition className.
    self
        writeInDirectoryName: definition className
        fileName: definition className
        extension: '.st'
        visit: [ self writeClassDefinition: definition ]
]

{ #category : #visiting }
MCFileTreeStWriter >> visitClassTraitDefinition: definition [
    orderedTraitNames add: definition className, ' classTrait'.
    self
        writeInDirectoryName: definition className
        fileName: definition className, '_classTrait'
        extension: '.st'
        visit: [ self writeClassTraitDefinition: definition ]
]

{ #category : #visiting }
MCFileTreeStWriter >> visitMethodDefinition: definition [
    | filename directoryname |
    directoryname := definition classIsMeta
        ifTrue: [ definition className , '_class' ]
        ifFalse: [ definition className ].
    filename := self fileNameForSelector: definition selector.
    self
        writeInDirectoryName: directoryname
        fileName: filename
        extension: '.st'
        visit: [ self writeMethodDefinition: definition ]
]

{ #category : #visiting }
MCFileTreeStWriter >> visitTraitDefinition: definition [
    orderedTraitNames add: definition className.
    self
        writeInDirectoryName: definition className
        fileName: definition className
        extension: '.st'
        visit: [ self writeTraitDefinition: definition ]
]
