"
MetacelloPharoPlatform contains all Pharo specific code to load Metacello.
"
Class {
	#name : 'MetacelloPharoCommonPlatform',
	#superclass : 'MetacelloPlatform',
	#category : 'Metacello-PharoCommonPlatform',
	#package : 'Metacello-PharoCommonPlatform'
}

{ #category : 'class initialization' }
MetacelloPharoCommonPlatform class >> initialize [
  self select
]

{ #category : 'notification' }
MetacelloPharoCommonPlatform >> collection: aCollection do: aBlock displaying: aString [

	self bypassProgressBars ifTrue: [ ^super collection: aCollection do: aBlock displaying: aString ].
	aCollection 
		do: aBlock
		displayingProgress: aString
]

{ #category : 'utilities' }
MetacelloPharoCommonPlatform >> compiler [
	^ self class compiler
]

{ #category : 'reflection' }
MetacelloPharoCommonPlatform >> copyClass: oldClass as: newName inCategory: newCategoryName [

	| copysName class newDefinition |
	copysName := newName asSymbol.
	copysName = oldClass name ifTrue: [ ^ oldClass ].
	(Smalltalk globals includesKey: copysName) ifTrue: [ ^ self error: copysName , ' already exists' ].
	newDefinition := oldClass definition copyReplaceAll: '#' , oldClass name asString with: '#' , copysName asString printString.
	newDefinition := newDefinition
		                 copyReplaceAll: 'category: ' , oldClass category asString printString
		                 with: 'category: ' , newCategoryName printString.
	class := self compiler
		         logged: true;
		         evaluate: newDefinition.
	class class instanceVariableNames: oldClass class instanceVariablesString.
	class copyAllMethodsFrom: oldClass.
	class class copyAllMethodsFrom: oldClass class.
	class category: newCategoryName.
	^ class
]

{ #category : 'notification' }
MetacelloPharoCommonPlatform >> do: aBlock displaying: aString [

    self bypassProgressBars ifTrue: [ ^ super do: aBlock displaying: aString ].
	
    aString
        displayProgressFrom: 0 to: 2
        during: [:bar |
            bar value: 1.
            aBlock value.
            bar value: 2 ]
]

{ #category : 'file system' }
MetacelloPharoCommonPlatform >> tempFileFor: aName suffix: aSuffixString [
	^ (FileLocator temp asFileReference
		/ (FileReference newTempFilePrefix: aName suffix: aSuffixString) basename)
		fullName
]

{ #category : 'utilities' }
MetacelloPharoCommonPlatform >> timestamp [

	^Date today mmddyyyy, ' ',
		((String streamContents: [:s | Time now print24: true on: s]) copyFrom: 1 to: 5)
]
