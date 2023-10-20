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

{ #category : 'attributes' }
MetacelloPharoCommonPlatform >> defaultPlatformAttributes [
	| attributes versionString |
	((Smalltalk respondsTo: #image) and: [ Smalltalk image respondsTo: #metacelloPlatformAttributes ])
		ifTrue: [ ^ Smalltalk image metacelloPlatformAttributes ].
	attributes := OrderedCollection with: #squeakCommon with: #pharo.
	Smalltalk
		at: #SystemVersion
		ifPresent: [ :cl | 
			versionString := cl current version.
			(((versionString beginsWith: 'Pharo-1') or: [ versionString beginsWith: 'PharoCore1' ])
				or: [ versionString beginsWith: 'Pharo1' ])
				ifTrue: [ 
					attributes add: #'pharo1.x'.
					((versionString beginsWith: 'Pharo-1.0') or: [ versionString beginsWith: 'PharoCore1.0' ])
						ifTrue: [ attributes add: #'pharo1.0.x' ]
						ifFalse: [ 
							((versionString beginsWith: 'Pharo-1.1') or: [ versionString beginsWith: 'Pharo1.1' ])
								ifTrue: [ attributes add: #'pharo1.1.x' ]
								ifFalse: [ 
									((versionString beginsWith: 'Pharo-1.2') or: [ versionString beginsWith: 'Pharo1.2' ])
										ifTrue: [ attributes add: #'pharo1.2.x' ]
										ifFalse: [ 
											(versionString beginsWith: 'Pharo1.3')
												ifTrue: [ attributes add: #'pharo1.3.x' ] ] ] ] ] ].
	^ attributes
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

{ #category : 'github/bitbucket support' }
MetacelloPharoCommonPlatform >> downloadJSON: url username: username pass: pass [
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  | client json |
  client := ZnClient new
    url: url;
    yourself.
  username ifNotNil: [ client username: username password: pass ].
  client get.
  json := client contents.
  ^ STON fromString: json
]

{ #category : 'monticello' }
MetacelloPharoCommonPlatform >> newVersionForWorkingCopy: aWorkingCopy [
	^aWorkingCopy newVersionIn: aWorkingCopy repositoryGroup
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
