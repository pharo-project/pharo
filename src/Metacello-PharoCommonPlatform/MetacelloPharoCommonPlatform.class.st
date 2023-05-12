"
MetacelloPharoPlatform contains all Pharo specific code to load Metacello.
"
Class {
	#name : #MetacelloPharoCommonPlatform,
	#superclass : #MetacelloPlatform,
	#category : #'Metacello-PharoCommonPlatform'
}

{ #category : #utilities }
MetacelloPharoCommonPlatform >> authorName [
	^ Author fullName
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> authorName: aString [

	Author fullName: aString
]

{ #category : #'github support' }
MetacelloPharoCommonPlatform >> closeZipArchive: anArchive [
	
	anArchive close
]

{ #category : #notification }
MetacelloPharoCommonPlatform >> collection: aCollection do: aBlock displaying: aString [

	self bypassProgressBars ifTrue: [ ^super collection: aCollection do: aBlock displaying: aString ].
	aCollection 
		do: aBlock
		displayingProgress: aString
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> compiler [
	^ Smalltalk compilerClass
]

{ #category : #reflection }
MetacelloPharoCommonPlatform >> copyClass: oldClass as: newName inCategory: newCategoryName [

	| copysName class newDefinition |
	copysName := newName asSymbol.
	copysName = oldClass name ifTrue: [ ^ oldClass ].
	(Smalltalk globals includesKey: copysName) ifTrue: [ ^ self error: copysName , ' already exists' ].
	newDefinition := oldClass definition copyReplaceAll: '#' , oldClass name asString with: '#' , copysName asString printString.
	newDefinition := newDefinition
		                 copyReplaceAll: 'category: ' , (Smalltalk organization categoryOfBehavior: oldClass) asString printString
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

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> createFiletreeRepository: aRepositorySpec [
	MCRepository 
		findFiletreeAlternateFormat: aRepositorySpec
		ifFound: [ :repository | 
			^ repository createRepositoryFromSpec: aRepositorySpec on: self ].

	^ super createFiletreeRepository: aRepositorySpec
]

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> createFtpRepository: aRepositorySpec [
 	| description headerSize index host directory |
	
	description := aRepositorySpec description.
	headerSize := 'ftp://' size.
	index := description indexOf: $/ startingAt: headerSize + 1.
	host := description copyFrom: headerSize + 1 to: index - 1.
	directory := description copyFrom: index + 1 to: description size.
	
	^ MCFtpRepository
		host: host 
		directory: directory 
		user: aRepositorySpec username
		password: aRepositorySpec password
]

{ #category : #attributes }
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

{ #category : #notification }
MetacelloPharoCommonPlatform >> do: aBlock displaying: aString [

    self bypassProgressBars ifTrue: [ ^ super do: aBlock displaying: aString ].
	
    aString
        displayProgressFrom: 0 to: 2
        during: [:bar |
            bar value: 1.
            aBlock value.
            bar value: 2 ]
]

{ #category : #'git-based http repo support' }
MetacelloPharoCommonPlatform >> downloadBasicFile: url to: outputFileName username: username pass: pass [
	"download from <url> into <outputFileName>"

	| out err proc errorFileName args |
	"Make a unique name for the error log and ensure that we can write the files"
	errorFileName := self downloadErrorFileNameFor: outputFileName.
	out := (outputFileName asFileReference)
		ensureDelete;
		writeStream.
	err := (errorFileName asFileReference)
		ensureDelete;
		writeStream.
	args := username
		ifNotNil: [ {'-u' . (username , ':' , pass) . '-L' . url} ]
		ifNil: [ {'-L' . url} ].
	proc := (self class environment at: #OSProcess) thisOSProcess
		forkJob: '/usr/bin/curl'
		arguments: args
		environment: nil
		descriptors: (Array with: nil with: out with: err).
	proc ifNil: [ (self class environment at: #OSProcess) noAccessorAvailable ].
	[ proc isRunning ] whileTrue: [ (Delay forMilliseconds: 100) wait ].
	out close.
	err close.
	^ errorFileName
]

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> extractTypeFromDescription: description [

	(description beginsWith: 'ftp://') ifTrue: [ ^'ftp' ].
	^super extractTypeFromDescription: description
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> timestamp [

	^Date today mmddyyyy, ' ',
		((String streamContents: [:s | Time now print24: true on: s]) copyFrom: 1 to: 5)
]
