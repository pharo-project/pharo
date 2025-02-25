"
My singleton is a central entry point to the system.

It has many roles and responsibilities (to be documented)
- startup/shutdown image
- source management
- namespace access
- tool access
- platform access
- VM information and parameters	

###Startup

At startup and shutdown the image execute the methods startUp: and shutdown: of registered classes (registered using addToStartUpList:, addToShutDownList:, ... methods and friends).
	
Startup phases
During the first stage of start up the UI manager, the default uimanager is switched to a specific non interactive ui manager (`StartupUIManager`). Note that this specific non interactive UIManager kills the system on any attempt to open windows and interaction. So be warned, don't use interaction in the first phase. 
Then all registered classes execute their start up procedures (which should not imply interactive behavior). 
After startup list is finished, any deferred startup actions are executed, which you can add using the method
`addDeferredStartupAction:` method.  

"
Class {
	#name : 'SmalltalkImage',
	#superclass : 'Object',
	#instVars : [
		'globals',
		'specialObjectsArray',
		'vm'
	],
	#classVars : [
		'CompilerClass',
		'LastImagePath',
		'LastQuitLogPosition',
		'LogFileName',
		'LowSpaceProcess',
		'LowSpaceSemaphore',
		'MemoryHogs',
		'SourceFileVersionString',
		'SpecialSelectors',
		'Tools'
	],
	#category : 'System-Support-Image',
	#package : 'System-Support',
	#tag : 'Image'
}

{ #category : 'cleanup' }
SmalltalkImage class >> cleanUp [

	Smalltalk image cleanOutUndeclared
]

{ #category : 'settings' }
SmalltalkImage class >> compilerClass [

	^CompilerClass ifNil: [
		"use OpalCompiler as the fallback compiler if installed"
		self class environment at: #OpalCompiler ifAbsent: [ nil ] ]
]

{ #category : 'settings' }
SmalltalkImage class >> compilerClass: aClass [
	CompilerClass := aClass
]

{ #category : 'instance creation' }
SmalltalkImage class >> current [
	^Smalltalk
]

{ #category : 'instance creation' }
SmalltalkImage class >> new [

	self error: 'Use current'
]

{ #category : 'system startup' }
SmalltalkImage class >> shutDown: isImageClosing [

	isImageClosing ifTrue: [ SourceFiles close ]
]

{ #category : 'system startup' }
SmalltalkImage class >> startUp: isImageStarting [

	ChangesLog default
]

{ #category : 'image' }
SmalltalkImage class >> wordSize [
	^ Smalltalk vm wordSize
]

{ #category : 'to clean later' }
SmalltalkImage >> allClasses [
	"Return all the class defines in the SmalltalkImage"
	"Smalltalk allClasses"
	"May be we could discard this one in the future"
	^ globals allClasses
]

{ #category : 'class and trait names' }
SmalltalkImage >> allClassesAndTraits [
	"Return all the classes and traits defined in the Smalltalk SmalltalkImage"

	^ globals allClassesAndTraits
]

{ #category : 'class and trait names' }
SmalltalkImage >> allClassesAndTraitsDo: aBlock [
	^globals allClassesAndTraitsDo: aBlock
]

{ #category : 'to clean later' }
SmalltalkImage >> allClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class in the system."

	globals allClassesDo: aBlock
]

{ #category : 'to clean later' }
SmalltalkImage >> allTraits [
	"Return all traits defined in the Smalltalk SmalltalkImage"

	^globals allTraits
]

{ #category : 'to clean later' }
SmalltalkImage >> argumentAt: i [
	"Answer the i-th argument of the command line, or nil if not so many argument."

	^self vm getSystemAttribute: 2 + i
]

{ #category : 'system attribute' }
SmalltalkImage >> arguments [
	"Answer an array with all the command line arguments.
	This does not include imagePath, documentPath nor any option."

	"Smalltalk commandLine arguments"

	^ self argumentsStartingAtIndex: 1
]

{ #category : 'private' }
SmalltalkImage >> argumentsInclude: aString [

	^ (self argumentsStartingAtIndex: 0) includes: aString
]

{ #category : 'private' }
SmalltalkImage >> argumentsStartingAtIndex: anIndex [
	"Answer an array with all the command line arguments"

	^ Array
		streamContents: [ :str |
			| arg i |
			i := anIndex.
			[ i > 998 or: [ (arg := self argumentAt: i) isNil ] ]
				whileFalse: [
					str nextPut: arg.
					i := i + 1 ] ]
]

{ #category : 'to clean later' }
SmalltalkImage >> at: aKey [
	"Answer the global associated with the key."

	^globals at: aKey
]

{ #category : 'to clean later' }
SmalltalkImage >> at: key ifAbsent: aBlock [
	"Answer the global associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	^globals at: key ifAbsent: aBlock
]

{ #category : 'to clean later' }
SmalltalkImage >> at: aKey ifAbsentPut: aBlock [
	"Return the global at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	^globals at: aKey ifAbsentPut: aBlock
]

{ #category : 'to clean later' }
SmalltalkImage >> at: key ifPresent: aBlock [
	"Lookup the given key in the globals. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	^globals at: key ifPresent: aBlock
]

{ #category : 'to clean later' }
SmalltalkImage >> at: key ifPresent: aBlock ifAbsent: anotherBlock [
	"Lookup the given key in the globals. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise answer the value of the second block."

	^ globals at: key ifPresent: aBlock ifAbsent: anotherBlock
]

{ #category : 'to clean later' }
SmalltalkImage >> at: aKey put: anObject [
	"Set the global at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	^globals at: aKey put: anObject
]

{ #category : 'saving' }
SmalltalkImage >> backupImageInFileNamed: aFileReference [
	"Save the  the image under the given file.
	Unlike #saveImageInFileNamed: this method continues evaluation in the original image after saving a backup copy.
	Note: This method does not save the .changes file, use #saveAs:"

	| imageFile previousFullImageName snapshotReturn |

	imageFile := aFileReference asFileReference.
	imageFile parent ensureCreateDirectory.
	previousFullImageName := self imagePath.

	[
		"change to the new image name "
		self
			changeImagePathTo: imageFile fullName;
			closeSourceFiles;
			openSourceFiles.  "so SNAPSHOT appears in new changes file"

		snapshotReturn := self snapshot: true andQuit: false.
	] ensure: [
		snapshotReturn isImageStarting ifFalse: [
			"unlike #saveImageInFileNamed: we do NOT continue in the new image"
			"switch back to the old image"
			self
				changeImagePathTo: previousFullImageName;
				closeSourceFiles;
				openSourceFiles ]].

	^ snapshotReturn
]

{ #category : 'saving' }
SmalltalkImage >> backupTo: newNameWithoutSuffix [
	"Create a new backup of this image.
	Unlike #saveAs:, I do not transfer the default execution to the new image.
	Results:
		true  when continuing in the new session
		false for the current session"
	(SourceFiles changesFileStream isNil or: [ SourceFiles changesFileStream closed ])
    ifFalse: [
		self
			closeSourceFiles; "so copying the changes file will always work"
			saveChangesInFileNamed: (self fullNameForChangesNamed: newNameWithoutSuffix)].

	^self backupImageInFileNamed: (self fullNameForImageNamed: newNameWithoutSuffix)
]

{ #category : 'to clean later' }
SmalltalkImage >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"

	^globals bindingOf: varName
]

{ #category : 'sources - date' }
SmalltalkImage >> buildDateString [

	^ 'Build date: ' , SystemVersion current date asString
]

{ #category : 'memory space' }
SmalltalkImage >> bytesLeft [
	"Answer the number of bytes of space available. Does a full garbage collection."

	^ self garbageCollect
]

{ #category : 'memory space' }
SmalltalkImage >> bytesLeft: aBool [
	"Return the amount of available space. If aBool is true, include possibly available swap space. If aBool is false, include possibly available physical memory. For a report on the largest free block currently available within Pharo memory but not counting extra memory use #primBytesLeft."
	<primitive: 112>
	^self primBytesLeft
]

{ #category : 'image, changes name' }
SmalltalkImage >> changeImagePathTo: aString [

	self imagePath: aString.
	LastImagePath := self imagePath
]

{ #category : 'image, changes name' }
SmalltalkImage >> changesName [
	"Answer the name for the changes file corresponding to the image file name."
	"Smalltalk changesName"

	^ self changesFile fullName
]

{ #category : 'image, changes name' }
SmalltalkImage >> changesSuffix [
	^ 'changes'
]

{ #category : 'class and trait names' }
SmalltalkImage >> classNamed: className [
	^ globals classOrTraitNamed: className
]

{ #category : 'class and trait names' }
SmalltalkImage >> classNames [
	^globals classNames
]

{ #category : 'class and trait names' }
SmalltalkImage >> classOrTraitNamed: aString [
	"aString is either a class or trait name or a class or trait name followed by ' class' or 'classTrait' respectively. Answer the class or metaclass it names."

	^ globals classOrTraitNamed: aString
]

{ #category : 'housekeeping' }
SmalltalkImage >> cleanOutUndeclared [
	| unreferenced |
	unreferenced := self environment undeclaredRegistry keys asSet.
	self systemNavigation allBehaviorsDo: [ :class |
		class methodsDo: [ :method |
			method withAllNestedLiteralsDo: [ :lit |
				(lit isKindOf: UndeclaredVariable) ifTrue: [
					unreferenced
						remove: lit key
						ifAbsent: [ "Sanity check--a binding with more than one reference may already be gone from the unreferenced set, but should be present in Undeclared itself"
							self environment undeclaredRegistry at: lit key ] ] ] ] ].

	unreferenced do: [ :key | self environment undeclaredRegistry removeKey: key ]
]

{ #category : 'cleanup' }
SmalltalkImage >> cleanUp [
	"Smalltalk cleanUp"
	"Gently clean up"

	^self cleanUp: false
]

{ #category : 'cleanup' }
SmalltalkImage >> cleanUp: aggressive [
	"Clean up. When aggressive is true, this will destroy resources, etc."
	"Smalltalk cleanUp: false"
	"Smalltalk cleanUp: true"

	^self cleanUp: aggressive except: #()
]

{ #category : 'cleanup' }
SmalltalkImage >> cleanUp: aggressive except: exclusions [
	"Clean up. When aggressive is true, this will destroy resources, etc.
	Leave out any classes specifically listed in exclusions."

	"Smalltalk cleanUp: true except: {Point}"

	^self cleanUp: aggressive except: exclusions confirming: aggressive
]

{ #category : 'cleanup' }
SmalltalkImage >> cleanUp: aggressive except: exclusions confirming: aBool [
	"Clean up. When aggressive is true, this will destroy resources, etc.
	Leave out any classes specifically listed in exclusions."

	"Smalltalk cleanUp: true except: {Point}
		- will ask for confirmation"
	"SmalltalkImage current cleanUp: true except: {} confirming: false
		- will not ask for confirmation"

	| classes |
	aBool ifTrue:[
		"Give the user a chance to bail"
		(self confirm: 'Cleanup will destroy change sets and more.
Are you sure you want to proceed?') ifFalse: [^self]].

	"Find all classes implementing #cleanUp or cleanUp:"
	classes := Smalltalk allClasses select: [:aClass|
		(aClass class includesSelector: #cleanUp)
			or: [aClass class includesSelector: #cleanUp:]].

	"Leave out the classes in the exclusion set"
	classes := classes reject: [:aClass| exclusions includes: aClass].

	"Arrange classes in superclass order, superclasses before subclasses.
	This will ensure that specific cleanup (like MethodDictionary compaction)
	will run after generic superclass cleanup (HashedCollection rehashing).
	Otherwise generic superclass cleanup might undo specific one (in this
	case rehashing will undo a good bit of MD compaction)."
	classes := Class superclassOrder: classes.

	"Run the cleanup code"
	classes
		do:[:aClass| aClass cleanUp: aggressive]
		displayingProgress: [:aClass| 'Cleaning up in ', aClass name]
]

{ #category : 'special objects' }
SmalltalkImage >> clearExternalObjects [
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Smalltalk clearExternalObjects"

	ExternalSemaphoreTable clearExternalObjects
]

{ #category : 'miscellaneous' }
SmalltalkImage >> closeLog: logger [
	"This is a _private_ method,
	Because it really belongs to logging facility,
	we should delegate to it at some point "

	logger ifNotNil: [ logger close ]
]

{ #category : 'sources, change log' }
SmalltalkImage >> closeSourceFiles [
	"Shut down the source files if appropriate."

	SourceFiles close
]

{ #category : 'system attribute' }
SmalltalkImage >> commandLine [
	"Answer the object to query about command line."

	^self
]

{ #category : 'special objects' }
SmalltalkImage >> compactClassesArray [
	"Backward-compatibility support.  Spur does not have compact classes."
	^{}
]

{ #category : 'compiler' }
SmalltalkImage >> compiler [
	^self compilerClass new
		environment: self globals
]

{ #category : 'accessing' }
SmalltalkImage >> compilerClass [
	^ self class compilerClass
]

{ #category : 'accessing' }
SmalltalkImage >> compilerClass: aClass [

	self class compilerClass: aClass
]

{ #category : 'memory space' }
SmalltalkImage >> createStackOverflow [
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; createStackOverflow"

	self createStackOverflow.  "infinite recursion"
]

{ #category : 'sources, change log' }
SmalltalkImage >> datedVersion [
	"Answer the version of this release."

	^SystemVersion current datedVersion
]

{ #category : 'log' }
SmalltalkImage >> defaultLogFileName [

	^ 'PharoDebug.log'
]

{ #category : 'accessing' }
SmalltalkImage >> environment [
	"For conversion from Smalltalk to SystemEnvironment"

	^globals
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> exit: exitStatus [
	"Primitive. Exit to another operating system on the host machine, if one
	exists. All state changes in the object space since the last snapshot are lost.
	Essential. See Object documentation whatIsAPrimitive.

	Possible values for exitStatus:
	0:   success
	> 1: error"

	<primitive: 113>
	self primitiveFailed
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> exitFailure [
	"Quit the VM with a failing signal.
	Will lose all current changes."
	self exit: 1
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> exitSuccess [
	"Quit the VM with a success signal.
	Will lose all current changes."
	self exit: 0
]

{ #category : 'miscellaneous' }
SmalltalkImage >> exitToDebugger [
	"Primitive. Enter the machine language debugger, if one exists. Essential.
	See Object documentation whatIsAPrimitive."

	<primitive: 114>
	self primitiveFailed
]

{ #category : 'special objects' }
SmalltalkImage >> externalObjects [
	"Return an array of objects that have been registered for use in non-Smalltalk code. Smalltalk objects should be referenced by external code only via indirection through this array, thus allowing the objects to move during compaction. This array can be cleared when the VM re-starts, since variables in external code do not survive snapshots. Note that external code should not attempt to access a Smalltalk object, even via this mechanism, while garbage collection is in progress."
	"Smalltalk externalObjects"

	^ ExternalSemaphoreTable externalObjects
]

{ #category : 'system attribute' }
SmalltalkImage >> extractMinusParameters [
	"Returns a dictionary matching parameters beginning by a minus character and the list of the next parameters.
	Here is an example:
		I suppose I run pharo with: `pharo myImage.image 1 2 3 woah -minus1 'Hello' -minus2 -minus3 'Hello' 'World'`.
		This message will return a dictionary with:
			* --> #(1 2 3 'woah')
			minus1 --> #('Hello')
			minus2 --> #()
			minus3 --> #('Hello' 'World')"

	| args str index |

	args := OrderedCollection new.

	str := self getSystemAttribute: (index := 2).
	[ str isNil ] whileFalse: [ args add: str. str := self getSystemAttribute: (index := index + 1) ].

	^ self extractMinusParametersFrom: args
]

{ #category : 'system attribute' }
SmalltalkImage >> extractMinusParametersFrom: arguments [
	| dict previous previousWithMinus args |
	dict := Dictionary new.
	arguments isEmptyOrNil
		ifTrue: [ ^ dict ].
	args := OrderedCollection withAll: arguments.
	previous := previousWithMinus := args detect: [ :each | each first = $- ] ifNone: [ String space ].
	[ args isEmpty or: [ args first = previous ] ]
		whileFalse: [
			dict at: #* put: (dict at: #* ifAbsent: [ #() ]) , (Array with: args first).
			args removeFirst ].
	args
		do: [ :each |
			each first = $-
				ifTrue: [
					previous first = $-
						ifTrue: [ dict at: previous allButFirst put: #() ].
					previousWithMinus := each ]
				ifFalse: [
					dict
						at: previousWithMinus allButFirst
						put: (dict at: previousWithMinus allButFirst ifAbsent: [ #() ]) , (Array with: each) ].
			previous := each ].
	previous first = $-
		ifTrue: [ dict at: previous allButFirst put: #() ].
	^ dict
]

{ #category : 'system attribute' }
SmalltalkImage >> extractParameters [

	| pName value index dict |
	dict := Dictionary new.
	index := 3. "Muss bei 3 starten, da 2 documentName ist"
	[pName := self vm getSystemAttribute: index.
	pName isEmptyOrNil] whileFalse:[
		index := index + 1.
		value := self vm getSystemAttribute: index.
		value ifNil: [value := ''].
 		dict at: pName asUppercase put: value.
		index := index + 1].
	^dict
]

{ #category : 'image, changes name' }
SmalltalkImage >> fileForChangesNamed: aString [

	^(self imageDirectory resolve: aString), self changesSuffix
]

{ #category : 'image, changes name' }
SmalltalkImage >> fileForImageNamed: aString [

	^(self imageDirectory resolve: aString), self imageSuffix
]

{ #category : 'housekeeping' }
SmalltalkImage >> fixObsoleteBindings [
	CompiledMethod allInstances do: [ :method |
		| obsoleteBindings |

		"Added to check because sometimes the bootstrap fails to generate some methods."
		self assert: (method header isKindOf: SmallInteger) description: (method name , ' has an invalid header:' , method header asString).

		obsoleteBindings := method literals select: [ :literal |
				literal isVariableBinding and: [
					literal value isBehavior and: [
						literal value isObsolete ] ] ].
		obsoleteBindings do: [ :binding |
			| obsName realName realClass |
			obsName := binding value name.
			self
				trace: 'Binding: ';
				traceCr: obsName.
			realName := obsName copyReplaceAll: 'AnObsolete' with: ''.
			realClass := Smalltalk globals at: realName asSymbol ifAbsent: [ UndefinedObject ].
			binding key: binding key value: realClass ].
		"do not forget to flush the JIT cache"
		obsoleteBindings ifNotEmpty: [ method flushCache ] ]
]

{ #category : 'housekeeping' }
SmalltalkImage >> fixObsoleteReferences [
	"SmalltalkImage current fixObsoleteReferences.
	SystemNavigation new obsoleteBehaviors size > 0
		ifTrue: [ SystemNavigation new obsoleteBehaviors inspect.
			self error:'Still have obsolete behaviors. See inspector']"

	self
		garbageCollect;
		fixObsoleteBindings;
		fixObsoleteSharedPools
]

{ #category : 'housekeeping' }
SmalltalkImage >> fixObsoleteSharedPools [
	self cleanOutUndeclared.
	self allClasses
		select: [ :class |
			class sharedPools isEmptyOrNil not and: [
				class sharedPools contains: #isObsolete ] ]
		thenDo: [ :class |
			| obsolete |
			obsolete := class sharedPools select: #isObsolete.
			class sharedPools removeAll: obsolete ].

	"#sharedPools is a lazy initializer and most classes don't have a shared pool.
	Resetting the shared pools to nil should therefore save a bit of space."
	self allClasses
		select: [ :class | class sharedPools isEmpty ]
		thenDo: [ :class | class sharedPools: nil ]
]

{ #category : 'to clean later' }
SmalltalkImage >> flushClassNameCache [
	"Smalltalk flushClassNameCache"
	"Force recomputation of the cached list of class names."

	globals flushClassNameCache
]

{ #category : 'image, changes name' }
SmalltalkImage >> fullNameForChangesNamed: aString [

	^(self fileForChangesNamed: aString) fullName
]

{ #category : 'image, changes name' }
SmalltalkImage >> fullNameForImageNamed: aString [

	^(self fileForImageNamed: aString) fullName
]

{ #category : 'memory space' }
SmalltalkImage >> garbageCollect [
	"Primitive. Reclaims all garbage and answers the size of the largest free chunk in old space."

	^ self primitiveGarbageCollect
]

{ #category : 'memory space' }
SmalltalkImage >> garbageCollectMost [
	"Primitive. Reclaims recently created garbage (which is usually most of it) fairly quickly and answers the number of bytes of available space."

	<primitive: 131>
	^ self primBytesLeft
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> getFileNameFromUser [

	| givenName strippedName imageFile changesFile |
	givenName := UIManager default
		request: 'New File Name?'
		initialAnswer: (self imageFile basename copyUpToLast:  Path extensionDelimiter).
	strippedName := self stripImageExtensionFrom: givenName.
	strippedName isEmptyOrNil ifTrue: [^nil].
	imageFile := self fileForImageNamed: strippedName.
	changesFile := self fileForChangesNamed: strippedName.
	((imageFile exists or: [changesFile exists]) and:
		[(self confirm: ('{1} already exists. Overwrite?' format: {strippedName})) not])
			ifTrue: [^nil].
	^strippedName
]

{ #category : 'accessing' }
SmalltalkImage >> globals [
	"Answer the global SystemEnvironment"
	^globals
]

{ #category : 'private' }
SmalltalkImage >> globals: aSystemEnvironment [
	"Sets the system-wide globals"

	globals ifNotNil: [self error: 'Cannot overwrite existing globals'].
	globals := aSystemEnvironment
]

{ #category : 'memory space' }
SmalltalkImage >> growMemoryByAtLeast: numBytes [
	"Grow memory by at least the requested number of bytes.
	 Primitive.  Essential. Fail if no memory is available."
	<primitive: 180>
	(numBytes isInteger and: [numBytes > 0]) ifTrue:
		[OutOfMemory signal].
	^self primitiveFailed
]

{ #category : 'class and trait names' }
SmalltalkImage >> hasClassNamed: aString [
	"Answer whether there is a class of the given name, but don't intern aString if it's not already interned."

	^ globals hasClassNamed: aString
]

{ #category : 'compiler' }
SmalltalkImage >> hasCompiler [

	^ self compilerClass isNotNil
]

{ #category : 'image' }
SmalltalkImage >> image [
	"Answer the object to query about the current object memory and execution environment."

	^self
]

{ #category : 'image, changes name' }
SmalltalkImage >> imageDirectory [
	"Answer the directory containing the current image."

	^ self imagePath asFileReference parent
]

{ #category : 'image' }
SmalltalkImage >> imageFormatVersion [
	"Answer an integer identifying the type of image. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines."

	"Smalltalk image imageFormatVersion"

	<primitive: 'primitiveImageFormatVersion'>
	self notify: 'This virtual machine does not support the optional primitive #primitiveImageFormatVersion'.
	^''
]

{ #category : 'image, changes name' }
SmalltalkImage >> imagePath [
	"Answer the full path name for the current image."

	"SmalltalkImage current imagePath"

	^ self primImagePath asByteArray utf8Decoded
]

{ #category : 'image, changes name' }
SmalltalkImage >> imagePath: newPath [
	"Set the the full path name for the current image.  All further snapshots will use this."

	self primImagePath: newPath utf8Encoded asString
]

{ #category : 'image, changes name' }
SmalltalkImage >> imageSuffix [
	^ 'image'
]

{ #category : 'to clean later' }
SmalltalkImage >> includesKey: key [
	"Answer whether the receiver has a key equal to the argument, key."

	^globals includesKey: key
]

{ #category : 'housekeeping' }
SmalltalkImage >> informSpaceLeftAfterGarbageCollection [
	"Do a garbage collection, and report results to the user."

	"SmalltalkImage current informSpaceLeftAfterGarbageCollection"

	self inform: self spaceLeftAfterGarbageCollection
]

{ #category : 'memory space' }
SmalltalkImage >> installLowSpaceWatcher [
	"Start a process to watch for low-space conditions."

	"Smalltalk installLowSpaceWatcher"

	self primSignalAtBytesLeft: 0.	"disable low-space interrupts"
	LowSpaceProcess ifNotNil: [ LowSpaceProcess terminate ].
	LowSpaceProcess := [ self lowSpaceWatcher ] newProcess.
	LowSpaceProcess name: 'Low Space Watcher'.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess resume
]

{ #category : 'testing' }
SmalltalkImage >> isHeadless [
	"Check if vm were run with headless parameter.
	Different VMs for different platform have different multiple way(s) to indicate that"

	-1000 to: 1000 do: [ :n |
		(#('display=none' '-headless' '-vm-display-null' '-nodisplay' '--headless' '--vm-display-null' '--nodisplay')
			includes: (self vm getSystemAttribute: n))
				ifTrue: [ ^ true ]].
	^ false
]

{ #category : 'testing' }
SmalltalkImage >> isInteractive [
	"Check if vm were run with headless parameter.
	Different VMs for different platform have different multiple way(s) to indicate that"

	"non-headless mode is always interactive"
	self isHeadless ifFalse: [ ^ true ].

	-1000 to: 1000 do: [ :n |
		(#('-interactive') includes: (self vm getSystemAttribute: n))
			ifTrue: [ ^ true ]].

	^ false
]

{ #category : 'testing' }
SmalltalkImage >> isInteractiveGraphic [
	"Check if vm were run headless and with interactive (graphics) parameter.
	 Using the headless VM, the need of a GUI environment is indicated by sending --interactive
	 parameter to the image."

	"non-headless mode is always interactive"
	self isHeadless ifFalse: [ ^ true ].

	-1000 to: 1000 do: [ :n |
		(#('--interactive') includes: (self vm getSystemAttribute: n))
			ifTrue: [ ^ true ]].

	^ false
]

{ #category : 'printing' }
SmalltalkImage >> isSelfEvaluating [
	self == Smalltalk ifTrue: [^true].
	^super isSelfEvaluating
]

{ #category : 'accessing' }
SmalltalkImage >> lastImagePath [
	^ LastImagePath
]

{ #category : 'sources, change log' }
SmalltalkImage >> lastQuitLogPosition [
	^ LastQuitLogPosition
]

{ #category : 'sources, change log' }
SmalltalkImage >> lastQuitLogPosition: aNumber [
	"should be only use to ensure the transition from SystemEnvironment to SmalltalkImage, then be removed"

	LastQuitLogPosition := aNumber
]

{ #category : 'sources, change log' }
SmalltalkImage >> lastUpdateString [
	"SmalltalkImage current lastUpdateString"

	^'Build information: ', SystemVersion current asString
]

{ #category : 'sources, change log' }
SmalltalkImage >> licenseString [
	^ self licenseStringTemplate format: (Array with: Date today year asString)
]

{ #category : 'sources, change log' }
SmalltalkImage >> licenseStringTemplate [
	^ 'LICENSE

Licensed under the MIT License with parts under the Apache License.

Copyright (c) 2008-{1} The Pharo Project, and Contributors
Copyright (c) 2008-{1} Inria
Copyright (c) 1996-2008 Viewpoints Research Institute, and Contributors
Copyright (c) 1996 Apple Computer, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following conditions: The
above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

You may obtain a copy of the Apache License at
http://www.apache.org/licenses/LICENSE-2.0

Pharo uses icons by Mark James (http://www.famfamfam.com) under
Creative Commons Attribution 2.5 License.

Pharo uses icons by Eclipse under
Eclipse Public License - v 1.0
http://www.eclipse.org/legal/epl-v10.html

Pharo uses Source Code Pro Fonts by Adobe under
SIL Open Font License, v 1.1
http://scripts.sil.org/OFL

Pharo uses Open Sans by Steve Matteson under
Apache License, 2.0
http://www.apache.org/licenses/LICENSE-2.0.html

Pharo uses  Bitstream Vera Fonts
http://dejavu-fonts.org/wiki/index.php?title=License

Copyright (c) 2003 by Bitstream, Inc. All Rights Reserved.
Bitstream Vera is a trademark of Bitstream, Inc.
Permission is hereby granted, free of charge, to any person obtaining a copy of the fonts
accompanying this license ("Fonts") and associated documentation files (the "Font Software"),
to reproduce and distribute the Font Software, including without limitation the rights to use, copy,
merge, publish, distribute, and/or sell copies of the Font Software, and to permit persons to whom
the Font Software is furnished to do so, subject to the following conditions:
The above copyright and trademark notices and this permission notice shall be included in all
copies of one or more of the Font Software typefaces.
The Font Software may be modified, altered, or added to, and in particular the designs of glyphs
or characters in the Fonts may be modified and additional glyphs or characters may be added to
the Fonts, only if the fonts are renamed to names not containing either the words "Bitstream"
or the word "Vera".
This License becomes null and void to the extent applicable to Fonts or Font Software that
has been modified and is distributed under the "Bitstream Vera" names.
The Font Software may be sold as part of a larger software package but no copy of one or more
of the Font Software typefaces may be sold by itself.
THE FONT SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT OF COPYRIGHT, PATENT, TRADEMARK, OR OTHER RIGHT. IN NO EVENT
SHALL BITSTREAM OR THE GNOME FOUNDATION BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
INCLUDING ANY GENERAL, SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF THE USE OR INABILITY TO USE THE
FONT SOFTWARE OR FROM OTHER DEALINGS IN THE FONT SOFTWARE.
Except as contained in this notice, the names of Gnome, the Gnome Foundation,
and Bitstream Inc., shall not be used in advertising or otherwise to promote the sale,
use or other dealings in this Font Software without prior written authorization from
the Gnome Foundation or Bitstream Inc., respectively.
For further information, contact: fonts at gnome dot org.'
]

{ #category : 'miscellaneous' }
SmalltalkImage >> logDuring: aMonadicBlock [
	" for safe use, if stream is a file, it needs to be closed after use "
	| logStream |

	[
		logStream := self openLog.
		aMonadicBlock value: logStream.
	] ensure: [ self closeLog: logStream ]
]

{ #category : 'miscellaneous' }
SmalltalkImage >> logError: errMsg inContext: aContext [

	" we should think about integrating a toothpick here someday"
	self logStdErrorDuring: [ :stderr|
		"install the line end conversion and force initialize the converter"
		stderr
			nextPutAll: errMsg; cr;
			"reset the color"
			nextPut: Character escape; nextPutAll: '[0m'.
		aContext shortDebugStackOn: stderr.].

	self logDuring: [:logger |
		logger
			nextPutAll: 'THERE_BE_DRAGONS_HERE'; cr;
		  	nextPutAll: errMsg; cr.
			"just print the error message if no context is given"
			aContext ifNotNil: [
				aContext errorReportOn: logger ].
		" write some type of separator"
		logger nextPutAll: (String new: 79 withAll: $- ); cr; cr]
]

{ #category : 'log' }
SmalltalkImage >> logFileName [

	^ LogFileName ifNil: [ self defaultLogFileName ]
]

{ #category : 'log' }
SmalltalkImage >> logFileName: newName [

	LogFileName := newName
]

{ #category : 'miscellaneous' }
SmalltalkImage >> logStdErrorDuring: aBlock [
	| stderr |
	[
		"install the line end conversion and force initialize the converter"
		stderr := ZnNewLineWriterStream on: (ZnCharacterWriteStream
			on: Stdio stderr
			encoding: 'utf8').

		"log in red"
		stderr nextPut: Character escape; nextPutAll: '[31m'.
		"rund the loggin block"
		aBlock value: stderr.
		"reset the coloring"
		stderr nextPut: Character escape; nextPutAll: '[0m'.
	] on: Error do: [ :e| "we don't care if the logging to stdout fails..." ]
]

{ #category : 'miscellaneous' }
SmalltalkImage >> logStdOutDuring: aBlock [
	| stderr |
	"install the line end conversion and force initialize the converter"
	stderr := ZnNewLineWriterStream
		on: (ZnCharacterWriteStream on: Stdio stdout encoding: 'utf8').

	[aBlock value: stderr.]
		on: Error do: [ "If the block fails we don't do nothing. This can lead to recursive errors" ].

	stderr flush
]

{ #category : 'memory space' }
SmalltalkImage >> lowSpaceThreshold [
	"Return the low space threshold. When the amount of free memory (after garbage collection) falls below this limit, the system is in serious danger of completely exhausting memory and crashing. This limit should be made high enough to allow the user open a debugger to diagnose a problem or to save the image."

	^ 400000  "Enough for JIT compiler"
]

{ #category : 'memory space' }
SmalltalkImage >> lowSpaceWatcher [
	"Wait until the low space semaphore is signalled, then take appropriate
	actions. "
	| free preemptedProcess |
	self garbageCollectMost <= self lowSpaceThreshold
		ifTrue: [self garbageCollect <= self lowSpaceThreshold
				ifTrue: ["free space must be above threshold before
					starting low space watcher"
					^ self inform: 'Not enough memory to launch the lowSpaceWatcher.']].
	self specialObjectsArray at: 23 put: nil.
	"process causing low space will be saved here"
	LowSpaceSemaphore := Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.
	"enable low space interrupts"
	LowSpaceSemaphore wait.
	"wait for a low space condition..."
	self primSignalAtBytesLeft: 0.
	"disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess := nil.
	"The process that was active at the time of the low space interrupt."
	preemptedProcess := Smalltalk specialObjectsArray at: 23.
	Smalltalk specialObjectsArray at: 23 put: nil.
	"Note: user now unprotected until the low space watcher is re-installed"
	self memoryHogs isEmpty
		ifFalse: [free := self bytesLeft.
			self bytesLeft > free
				ifTrue: [^ self installLowSpaceWatcher]].

	UIManager default lowSpaceWatcherDefaultAction: preemptedProcess.
	"We need to reinstall the lowSpaceWatcher"
	Smalltalk installLowSpaceWatcher
]

{ #category : 'memory space' }
SmalltalkImage >> lowSpaceWatcherProcess [
	^LowSpaceProcess
]

{ #category : 'system attribute' }
SmalltalkImage >> maxFilenameLength [

	^self vm maxFilenameLength
]

{ #category : 'system attributes' }
SmalltalkImage >> maxIdentityHash [
	"Answer the maximum identityHash value supported by the VM."
	<primitive: 176>
	^self primitiveFailed
]

{ #category : 'memory space' }
SmalltalkImage >> memoryHogs [
	"Answer the list of objects to notify with #freeSomeSpace if memory gets full."

	^ MemoryHogs ifNil: [MemoryHogs := OrderedCollection new]
]

{ #category : 'miscellaneous' }
SmalltalkImage >> metacelloPlatformAttributes [
	"Returns the tags for the conditional platform loading in Metacello. Pay attention the order is important: from most  to least general."

	^ {
		  #squeakCommon.
		  #pharo.
		  ('pharo' , SystemVersion current major asString , '.x') asSymbol.
		  ('pharo' , SystemVersion current major asString , '.0.x') asSymbol }
]

{ #category : 'special objects' }
SmalltalkImage >> newSpecialObjectsArray [
	"Smalltalk recreateSpecialObjectsArray"

	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use,
	use addGCRoot() function and use own, separate special objects registry "

	"The Special Objects Array is an array of objects used by the virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 60.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	newArray at: 4 put: (self globals associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: (self globals at: #Bitmap ifAbsent: [nil]).
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: ByteString.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: BoxedFloat64.
	newArray at: 11 put: (self globals at: #MethodContext ifAbsent: [self globals at: #Context]).
	newArray at: 12 put: nil. "was BlockContext."
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: nil. "was Display."
	newArray at: 16 put: Message.
	newArray at: 17 put: nil. "was CompiledMethod."
	newArray at: 18 put: ((self primitiveGetSpecialObjectsArray at: 18) ifNil: [Semaphore new]). "low space Semaphore"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#'~~' 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	newArray at: 25 put: nil "was an array of 255 Characters in ascii order".
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: nil. "was Process."
	newArray at: 29 put: nil. "was compact classes"
	newArray at: 30 put: ((self primitiveGetSpecialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self primitiveGetSpecialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"
	newArray at: 32 put: Float32Array. "was the prototype Float"
	newArray at: 33 put: Float64Array. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"
	newArray at: 35 put: #cannotInterpret:.
	newArray at: 36 put: nil. "was the prototype MethodContext"
	newArray at: 37 put: nil. "was BlockClosure."
	newArray at: 38 put: nil. "was the prototype BlockContext"
	"array of objects referred to by external code"
	newArray at: 39 put: (self primitiveGetSpecialObjectsArray at: 39).	"external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self primitiveGetSpecialObjectsArray at: 41) ifNil: [ProcessList new]). "Reserved for a ProcessList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self primitiveGetSpecialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: nil. "was ExternalStructure"
	newArray at: 46 put: nil. "was ExternalData"
	newArray at: 47 put: nil.
	newArray at: 48 put: nil.
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	newArray at: 51 put: #attemptToAssign:withIndex:.
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object'
							#'object moved' #'object not pinned' #'callback error'),
							{PrimitiveError new errorName: #'operating system error'; yourself}.

	newArray at: 53 put: nil.
	newArray at: 54 put: nil.
	newArray at: 55 put: nil.

	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for foreign callback process"
	newArray at: 57 put: (self primitiveGetSpecialObjectsArray at: 57 ifAbsent: []).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	^newArray
]

{ #category : 'memory space' }
SmalltalkImage >> okayToProceedEvenIfSpaceIsLow [
	"Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning."

	self garbageCollectMost > self lowSpaceThreshold ifTrue: [^ true].  "quick"
	self garbageCollect > self lowSpaceThreshold ifTrue: [^ true].  "work harder"

	^ self confirm:
'WARNING: There is not enough space to start the low space watcher.
If you proceed, you will not be warned again, and the system may
run out of memory and crash. If you do proceed, you can start the
low space notifier when more space becomes available simply by
opening and then closing a debugger (e.g., by hitting Cmd-period.)
Do you want to proceed?'
]

{ #category : 'miscellaneous' }
SmalltalkImage >> openLog [
	"This is a _private_ method,
	Because it really belongs to logging facility,
	we should delegate to it at some point "

	^ [ZnNewLineWriterStream on: (ZnCharacterWriteStream
		on:
			((File named: Smalltalk logFileName) writeStream
				setToEnd;
				yourself)
		encoding: 'utf8')] on: FileException do: [ NullStream new ]
]

{ #category : 'sources, change log' }
SmalltalkImage >> openSourceFiles [

	self imagePath = LastImagePath ifFalse: [ "Reset the author full name to blank when the image gets moved"
		LastImagePath := self imagePath ].

	SourceFiles ensureOpen
]

{ #category : 'system attribute' }
SmalltalkImage >> optionAt: i [
	"Answer the i-th option of the command line, or nil if not so many options."

	^self  vm optionAt: i
]

{ #category : 'system attribute' }
SmalltalkImage >> options [
	"Answer an array with all the command line options."

	"Smalltalk commandLine options"

	^ Array
		streamContents: [ :str |
			| arg i |
			i := 1.
			[ i > 1000 or: [ (arg := self optionAt: i) isNil ] ]
				whileFalse: [
					str nextPut: arg.
					i := i + 1 ] ]
]

{ #category : 'class and trait names' }
SmalltalkImage >> organization [
	"Return the organizer for the receiver"
	^globals organization
]

{ #category : 'accessing' }
SmalltalkImage >> packages [

	^ self organization packages
]

{ #category : 'memory space' }
SmalltalkImage >> primBytesLeft [
	"Primitive. Answer the number of free bytes available in old space.
	 Not accurate unless preceded by
		Smalltalk garbageCollectMost (for reasonable accuracy), or
		Smalltalk garbageCollect (for real accuracy).
	 See Object documentation whatIsAPrimitive."

	<primitive: 112>
	^0
]

{ #category : 'primitives' }
SmalltalkImage >> primImagePath [
	"Answer the full path name for the current image."
	"Smalltalk imageName"

	<primitive: 121>
	self primitiveFailed
]

{ #category : 'primitives' }
SmalltalkImage >> primImagePath: newName [
	"Set the the full path name for the current image.  All further snapshots will use this."

	<primitive: 121>
	^ self primitiveFailed
]

{ #category : 'memory space' }
SmalltalkImage >> primLowSpaceSemaphore: aSemaphore [
	"Primitive. Register the given Semaphore to be signalled when the
	number of free bytes drops below some threshold. Disable low-space
	interrupts if the argument is nil."

	<primitive: 124>
	self primitiveFailed
]

{ #category : 'memory space' }
SmalltalkImage >> primSignalAtBytesLeft: numBytes [
	"Tell the interpreter the low-space threshold in bytes. When the free
	space falls below this threshold, the interpreter will signal the low-space
	semaphore, if one has been registered.  Disable low-space interrupts if the
	argument is zero.  Fail if numBytes is not an Integer."

	<primitive: 125>
	self primitiveFailed
]

{ #category : 'memory space' }
SmalltalkImage >> primitiveGarbageCollect [
	"Primitive. Reclaims all garbage and answers the size of the largest free chunk in old space."

	<primitive: 130>
	^self primitiveFailed
]

{ #category : 'special objects' }
SmalltalkImage >> primitiveGetSpecialObjectsArray [
	<primitive: 129>
	^ self primitiveFailed
]

{ #category : 'printing' }
SmalltalkImage >> printOn: aStream [
	self == Smalltalk ifFalse: [^super printOn: aStream].
	aStream nextPutAll: 'Smalltalk'
]

{ #category : 'private' }
SmalltalkImage >> privSaveImageWithCleanup [

	"Do not use this method for common image saving. This method is designated for saving the image during the shrinking process in headless images."

	20 timesRepeat: [
		(Delay forMilliseconds: 50) wait.
		Processor yield.
		Smalltalk garbageCollect. ].

	[
		self snapshot: true andQuit: true.
	] onErrorDo: [:e |
		'PharoDebug.log' asFileReference ensureDelete; writeStreamDo: [ :rep |
			rep nextPutAll: 'Error:'; cr.
			rep nextPutAll: e asString; cr.
	  		rep nextPutAll: thisContext stack size asString.
	 	  	thisContext stack copy withIndexDo: [:stck :i |
				[ rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] onErrorDo: []]].
		Smalltalk exitFailure ]
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> quitPrimitive [
	"Primitive. Exit to another operating system on the host machine, if one
	exists. All state changes in the object space since the last snapshot are lost.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 113>
	self primitiveFailed
]

{ #category : 'compiler' }
SmalltalkImage >> recompile [
	Smalltalk globals allClassesAndTraits
		do: [:classOrTrait | classOrTrait recompile]
		displayingProgress: 'Recompiling all classes and traits'
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> recordStartupStamp [

	ChangesLog default recordStartupStamp
]

{ #category : 'special objects' }
SmalltalkImage >> recreateSpecialObjectsArray [
	"Smalltalk recreateSpecialObjectsArray"

	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use,
	use addGCRoot() function and use own, separate special objects registry "

	"The Special Objects Array is an array of objects used by the virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."

	"Replace the interpreter's reference in one atomic operation"
	self specialObjectsArray becomeForward: self newSpecialObjectsArray
]

{ #category : 'special objects' }
SmalltalkImage >> registerExternalObject: anObject [
	"Register the given object in the external objects array and return its index. If it is already there, just return its index."

	^ExternalSemaphoreTable registerExternalObject: anObject
]

{ #category : 'housekeeping' }
SmalltalkImage >> removeAllLineFeedsQuietly [
	"Smalltalk removeAllLineFeedsQuietly"
	"Scan all methods for source code with lineFeeds.
	Replaces all occurrences of <CR><LF> or <LF> by <CR>.
	Answer a Dictionary keyed by author name containing sets of affected method names,
	as well as (at the key 'OK') a list of methods that still contain LF characters inside literal strings or characters."
	^self removeAllLineFeedsQuietlyCalling: []
]

{ #category : 'housekeeping' }
SmalltalkImage >> removeAllLineFeedsQuietlyCalling: aBlock [
	"Smalltalk removeAllLineFeedsQuietly"
	"Scan all methods for source code with lineFeeds.
	Replaces all occurrences of <CR><LF> or <LF> by <CR>.
	Answer a Dictionary keyed by author name containing sets of affected method names,
	as well as (at the key 'OK') a list of methods that still contain LF characters inside literal strings or characters.
	Evaluate aBlock for each method so that status can be updated."
	| authors |
	authors := Dictionary new.
	authors at: 'OK' put: Set new.
	self systemNavigation
		allBehaviorsDo: [:cls | cls selectorsAndMethodsDo: [:selector :method |
					| oldStamp newCodeString oldCodeString oldProtocolName nameString |
					aBlock cull: cls cull: selector.
					oldCodeString := cls sourceCodeAt: selector.
					(oldCodeString includes: Character lf)
						ifTrue: [
							newCodeString := oldCodeString withInternalLineEndings.
							nameString := cls name , '>>' , selector.
							((cls compiledMethodAt: selector) hasLiteralSuchThat: [ :lit | lit asString includes: Character lf ])
								ifTrue: [(authors at: 'OK')
										add: nameString]
								ifFalse: [oldStamp := (method timeStamp)
												copy replaceAll: Character cr
												with: Character space.
									(authors
										at: (oldStamp copyFrom: 1 to: (oldStamp findFirst: [ :c | c isAlphaNumeric not ]))
										ifAbsentPut: [Set new])
										add: nameString.
									oldProtocolName := cls protocolNameOfSelector: selector.
									cls
										compile: newCodeString
										classified: oldProtocolName
										withStamp: oldStamp
										notifying: nil ]]]].
	^ authors
]

{ #category : 'class and trait names' }
SmalltalkImage >> removeClassNamed: aName [
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "

	globals removeClassNamed: aName
]

{ #category : 'housekeeping' }
SmalltalkImage >> removeEmptyMessageCategories [
	<script>
	
	SystemNavigation default allBehaviorsDo: [ :class | class removeEmptyProtocols ].
	self packageOrganizer removeEmptyPackagesAndTags
]

{ #category : 'shrinking' }
SmalltalkImage >> removeSelector: descriptor [
	"Safely remove a selector from a class (or metaclass). If the
	class or the method doesn't exist anymore, never mind and
	answer nil.
	This method should be used instead of 'Class removeSelector:
	#method' to omit global class references."
	| class sel |
	class := self
				at: descriptor first
				ifAbsent: [^ nil].
	(descriptor size > 2 and: [descriptor second == #class])
		ifTrue:
			[class := class class.
			sel := descriptor third]
		ifFalse:
			[sel := descriptor second].
	^ class removeSelector: sel
]

{ #category : 'class and trait names' }
SmalltalkImage >> renameClass: aClass from: oldName [
	"Rename the class, aClass, to have the title newName."

	^globals renameClass: aClass from: oldName
]

{ #category : 'class and trait names' }
SmalltalkImage >> renameClassNamed: oldName as: newName [
	"Invoked from fileouts:  if there is currently a class in the system named oldName, then rename it to newName.  If anything untoward happens, report it in the Transcript.  "

	^ globals renameClassNamed: oldName as: newName
]

{ #category : 'cleanup' }
SmalltalkImage >> restartMethods [

	"Clean up. Long running loops or stored closures can lead to methods that are out if sync with the recompiled code"

	| classes |
	"Find all classes implementing #restartMethods"
	classes := self allClasses select: [ :aClass |
		           aClass class includesSelector: #restartMethods ].
	"Arrange classes in superclass order, superclasses before subclasses"
	classes := Class superclassOrder: classes.
	"Run the cleanup code"
	classes do: [ :aClass | aClass restartMethods ]
]

{ #category : 'saving' }
SmalltalkImage >> saveAs [
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self saveAs: self getFileNameFromUser
]

{ #category : 'saving' }
SmalltalkImage >> saveAs: newNameWithoutSuffix [
	"Results:
		true  when continuing in the new session
		false for the current session "

	newNameWithoutSuffix ifNil: [ ^ self ].
  (SourceFiles changesFileStream isNil or: [ SourceFiles changesFileStream closed ])
    ifFalse: [
		self closeSourceFiles.
		self saveChangesInFileNamed: (self fullNameForChangesNamed: newNameWithoutSuffix)
		"so copying the changes file will always work" ].

	^ self saveImageInFileNamed: (self fullNameForImageNamed: newNameWithoutSuffix)
]

{ #category : 'saving' }
SmalltalkImage >> saveAsNewVersion [
	"Save the image/changes using the next available version number."

	"SmalltalkImage current saveAsNewVersion"

	| newImageFile strippedName |
	newImageFile := self imageFile nextVersion.
	(newImageFile withExtension: self changesSuffix)
		ifExists: [ :newChangesFile |
			^ self
				inform:
					'There is already .changes file of the desired name,
' , newChangesFile fullName , '
curiously already present, even though there is
no corresponding .' , self imageSuffix , ' file.   Please remedy
manually and then repeat your request.' ].

	strippedName := self stripImageExtensionFrom: newImageFile fullName.
	^ self saveAs: strippedName
]

{ #category : 'saving' }
SmalltalkImage >> saveChangesInFileNamed: aString [
	| changesFile |
	changesFile := aString asFileReference.
	changesFile parent ensureCreateDirectory.
	"Ensure that the destination file is not the same as the original one"
	(Smalltalk changesFile absolutePath = changesFile absolutePath)
		ifFalse: [
			changesFile deleteIfAbsent: [  ].
			Smalltalk changesFile copyTo: changesFile ]
]

{ #category : 'saving' }
SmalltalkImage >> saveImageInFileNamed: aFile [
	"Save the image in the given file.
	Unlike #backupImageInFileNamed: this method continues evaluation in the new image after saving.
	Note: This method does not save the .changes file, use #saveAs:"

	| imageFile |

	imageFile := aFile  asFileReference.
	imageFile parent ensureCreateDirectory.

	^self
		changeImagePathTo: imageFile fullName;
		closeSourceFiles;
		openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false
]

{ #category : 'saving' }
SmalltalkImage >> saveImageInNewContext [

	" This method is used for saving the Smalltalk image in a new process so the current process with all stack information may be forgotten.

	Do not use this method for common image saving. This method is designated for saving the image during the shrinking process in headless images. "

	[ self privSaveImageWithCleanup ] forkAt: 40
]

{ #category : 'saving' }
SmalltalkImage >> saveSession [
	"Save the current image without quitting and return it resuming status (whether it was restarted or in the same flow)."

	^ self snapshot: true andQuit: false
]

{ #category : 'image, changes name' }
SmalltalkImage >> shortImageName [
	"Answer the current image name but only that name and not the full path as with imageName."
	"SmalltalkImage current shortImageName"

	^ self imageFile basename
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> shutDown [
	^ self closeSourceFiles
]

{ #category : 'memory space' }
SmalltalkImage >> signalLowSpace [
	"Signal the low-space semaphore to alert the user that space is running low."

	LowSpaceSemaphore signal
]

{ #category : 'snapshot and quit' }
SmalltalkImage >> snapshot: save andQuit: quit [
	DefaultExecutionEnvironment beActiveDuring:
		 [^SessionManager default snapshot: save andQuit: quit]
]

{ #category : 'image, changes name' }
SmalltalkImage >> sourceFileVersionString [

	^ SourceFileVersionString
]

{ #category : 'private' }
SmalltalkImage >> sourceFileVersionString: aString [

	SourceFileVersionString := aString
]

{ #category : 'image, changes name' }
SmalltalkImage >> sourcesFile [
	"Answer the full path to the version-stable source code"
	^ self imageDirectory / (SourceFileVersionString , '.', self sourcesSuffix)
]

{ #category : 'image, changes name' }
SmalltalkImage >> sourcesName [
	"Answer the full path to the version-stable source code"
	^ self sourcesFile fullName
]

{ #category : 'image, changes name' }
SmalltalkImage >> sourcesSuffix [
	^ 'sources'
]

{ #category : 'memory space' }
SmalltalkImage >> spaceLeftAfterGarbageCollection [
	"Return a string describing the amount of memory available"
	| availInternal availPhysical availTotal |
	self garbageCollect.
	availInternal := self primBytesLeft.
	availPhysical := self bytesLeft: false.
	availTotal := self bytesLeft: true.
	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"
		ifFalse:[^availInternal asStringWithCommas, ' bytes available'].
	^String streamContents:[:s|
		availInternal printWithCommasOn: s.
		s nextPutAll: ' bytes (internal) '; cr.
		 availPhysical printWithCommasOn: s.
		s nextPutAll: ' bytes (physical) '; cr.
		availTotal printWithCommasOn: s.
		s nextPutAll: ' bytes (total)     ']
]

{ #category : 'special objects' }
SmalltalkImage >> specialNargsAt: anInteger [
	"Answer the number of arguments for the special selector at: anInteger."

	^ (self specialObjectsArray at: 24) at: anInteger * 2
]

{ #category : 'special objects' }
SmalltalkImage >> specialObjectsArray [
	^specialObjectsArray ifNil: [ specialObjectsArray := self primitiveGetSpecialObjectsArray ]
]

{ #category : 'special objects' }
SmalltalkImage >> specialObjectsArray: anArray [
	specialObjectsArray := anArray
]

{ #category : 'special objects' }
SmalltalkImage >> specialSelectorAt: anInteger [
	"Answer the special message selector from the interleaved specialSelectors array."

	^ (self specialObjectsArray at: 24) at: anInteger * 2 - 1
]

{ #category : 'special objects' }
SmalltalkImage >> specialSelectorIndexOrNil: selector [
	1 to: Smalltalk specialSelectorSize do:
		[:index |
		(Smalltalk specialSelectorAt: index) == selector
			ifTrue: [^index ]].
	^ nil
]

{ #category : 'special objects' }
SmalltalkImage >> specialSelectorSize [
	"Answer the number of special selectors in the system."

	^ (self specialObjectsArray at: 24) size // 2
]

{ #category : 'special objects' }
SmalltalkImage >> specialSelectors [
	"Used by SystemTracer only."

	^SpecialSelectors
]

{ #category : 'saving' }
SmalltalkImage >> stripImageExtensionFrom: aString [

	| answer imageExtension delimitedImageExtension |
	answer := aString.
	imageExtension := Path extensionDelimiter asString, self imageSuffix.
	delimitedImageExtension := imageExtension copyWith: Path extensionDelimiter.
	[answer isNil not and: [(answer endsWith: imageExtension) or: [answer endsWith: delimitedImageExtension]]]
		whileTrue: [answer := answer copyUpToLast: Path extensionDelimiter].
	^answer
]

{ #category : 'system attributes' }
SmalltalkImage >> supportsQueueingFinalization [
	"Determine whether the VM queues individual weak arrays for finalization, instead
	 of signalling the finalization semaphore once for all arrays and having the
	 WeakRegistry mechanism finalize all weak arrays, whether they need to or not.
	 This flag persists across snapshots, stored in the image header."
	"SmalltalkImage current supportsQueueingFinalization"

	^ ((self vm parameterAt: 48) bitAnd: 16) = 16
]

{ #category : 'system attributes' }
SmalltalkImage >> supportsQueueingFinalization: aBoolean [
	"Determine whether the VM queues individual weak arrays for finalization, instead
	 of signalling the finalization semaphore once for all arrays and having the
	 WeakRegistry mechanism finalize all weak arrays, whether they need to or not.
	 This flag persists across snapshots, stored in the image header."
	"SmalltalkImage current supportsQueueingFinalization: true"

	self vm parameterAt: 48 put: ((self vm parameterAt: 48) bitClear: 16) + (aBoolean ifTrue: [16] ifFalse: [0])
]

{ #category : 'sources, change log' }
SmalltalkImage >> systemInformationString [

	^ String cr , 'Pharo ' , SystemVersion current dottedMajorMinorPatch
	  , String cr , self lastUpdateString , String cr
	  , self buildDateString , String cr , String cr , self licenseString
]

{ #category : 'sources, change log' }
SmalltalkImage >> timeStamp: aStream [
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime := DateAndTime now.
	aStream
		nextPutAll: 'From ' , self datedVersion , ' [' , self lastUpdateString , '] on ' , dateTime asDate printString;
		nextPutAll: ' at ';
		nextPutAll: dateTime asTime printString
]

{ #category : 'class and trait names' }
SmalltalkImage >> traitNames [
	"Answer a SortedCollection of all traits (not including class-traits) names."

	^ globals traitNames
]

{ #category : 'external' }
SmalltalkImage >> unbindExternalPrimitives [
	"Primitive. Force all external primitives to be looked up again afterwards. Since external primitives that have not found are bound for fast failure this method will force the lookup of all primitives again so that after adding some plugin the primitives may be found."
	<primitive: 570>
	"Do nothing if the primitive fails for compatibility with older VMs"
]

{ #category : 'special objects' }
SmalltalkImage >> unregisterExternalObject: anObject [
	"Unregister the given object in the external objects array. Do nothing if it isn't registered."

	ExternalSemaphoreTable unregisterExternalObject: anObject
]

{ #category : 'sources, change log' }
SmalltalkImage >> version [
	"Answer the version of this release."

	^SystemVersion current version
]

{ #category : 'copying' }
SmalltalkImage >> veryDeepCopyWith: deepCopier [
	"Return self.  I can't be copied.  Do not record me."
]

{ #category : 'accessing' }
SmalltalkImage >> vm [
	"Answer the object to query about virtual machine."

	^ vm ifNil: [vm := VirtualMachine new]
]

{ #category : 'image, changes name' }
SmalltalkImage >> vmBinary [
	^ self vm binary
]

{ #category : 'image, changes name' }
SmalltalkImage >> vmDirectory [
	^ self vm directory
]

{ #category : 'image' }
SmalltalkImage >> wordSize [
	"Answer the size in bytes of an object pointer or word in the object memory.
	The value does not change for a given image, but may be modified by a SystemTracer
	when converting the image to another format. The value is cached in WordSize to
	avoid the performance overhead of repeatedly consulting the VM."

	"this method is deprecated"
	^ self vm wordSize
]
