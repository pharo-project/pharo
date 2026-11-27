Class {
	#name : 'AutomaticRewriting',
	#superclass : 'Warning',
	#instVars : [
		'context',
		'explanationString',
		'deprecationDate',
		'versionString',
		'rule',
		'condition'
	],
	#classInstVars : [
		'log',
		'reaction',
		'rewriting'
	],
	#category : 'Deprecation-Exceptions',
	#package : 'Deprecation',
	#tag : 'Exceptions'
}

{ #category : 'settings - deprecated' }
AutomaticRewriting class >> activateTransformations [

	self deprecated: 'Use #rewriting, a DeprecationRewriting subclass' in: #Pharo15.
	^ self rewriting ~~ NeverRewriteDeprecation
]

{ #category : 'settings - deprecated' }
AutomaticRewriting class >> activateTransformations: aBoolean [

	self deprecated: 'Use #rewriting: with a DeprecationRewriting subclass' in: #Pharo15.
	self rewriting: (aBoolean
		 ifTrue: [ self defaultRewriting ]
		 ifFalse: [ NeverRewriteDeprecation ])
]

{ #category : 'logging' }
AutomaticRewriting class >> addLog: anElement [ 

	self log add: anElement
]

{ #category : 'settings' }
AutomaticRewriting class >> defaultReaction [

	^ LogDeprecationReaction
]

{ #category : 'settings' }
AutomaticRewriting class >> defaultRewriting [

	^ CompatibilityPeriodRewriteDeprecation
]

{ #category : 'class initialization' }
AutomaticRewriting class >> initialize [
	self reset
]

{ #category : 'class initialization' }
AutomaticRewriting class >> log [ 

	^ log ifNil: [ log := OrderedCollection new. log ]
]

{ #category : 'settings - deprecated' }
AutomaticRewriting class >> raiseWarning [

	self deprecated: 'Use #reaction, a DeprecationReaction subclass' in: #Pharo15.
	^ self reaction == WarnDeprecationReaction
]

{ #category : 'settings - deprecated' }
AutomaticRewriting class >> raiseWarning: aBoolean [

	self deprecated: 'Use #reaction: with a DeprecationReaction subclass' in: #Pharo15.
	aBoolean ifTrue: [ ^ self reaction: WarnDeprecationReaction ].
	self reaction == WarnDeprecationReaction ifTrue: [ self reaction: LogDeprecationReaction ]
]

{ #category : 'settings' }
AutomaticRewriting class >> reaction [
	"The DeprecationReaction subclass that decides what happens when a deprecated method runs."

	^ reaction ifNil: [ reaction := self defaultReaction ]
]

{ #category : 'settings' }
AutomaticRewriting class >> reaction: aDeprecationReactionClass [

	reaction := aDeprecationReactionClass
]

{ #category : 'class initialization' }
AutomaticRewriting class >> reset [

	<script>
	log := nil.
	reaction := nil.
	rewriting := nil
]

{ #category : 'settings' }
AutomaticRewriting class >> rewriting [
	"The DeprecationRewriting subclass that decides whether a deprecation with a rewriting rule rewrites the caller."

	^ rewriting ifNil: [ rewriting := self defaultRewriting ]
]

{ #category : 'settings' }
AutomaticRewriting class >> rewriting: aDeprecationRewritingClass [

	rewriting := aDeprecationRewritingClass
]

{ #category : 'settings - deprecated' }
AutomaticRewriting class >> showWarning [

	self deprecated: 'Use #reaction, a DeprecationReaction subclass' in: #Pharo15.
	^ self reaction == LogDeprecationReaction or: [ self reaction == WarnDeprecationReaction ]
]

{ #category : 'settings - deprecated' }
AutomaticRewriting class >> showWarning: aBoolean [

	self deprecated: 'Use #reaction: with a DeprecationReaction subclass' in: #Pharo15.
	aBoolean ifTrue: [
		self reaction == IgnoreDeprecationReaction ifTrue: [ self reaction: LogDeprecationReaction ].
		^ self ].
	self reaction == LogDeprecationReaction ifTrue: [ self reaction: IgnoreDeprecationReaction ]
]

{ #category : 'comparing' }
AutomaticRewriting >> = anObject [
	^self class == anObject class
	  and: [context = anObject contextOfDeprecatedMethod
	  and: [context
			ifNil: [explanationString = anObject explanationString]
			ifNotNil: [true]]]
]

{ #category : 'compatibility period' }
AutomaticRewriting >> compatibilityPeriod [
	"PHEP-0004: number of Pharo versions, the one that deprecates included, that keep a deprecated API working."

	^ self isKernel
		  ifTrue: [ 4 ]
		  ifFalse: [ 2 ]
]

{ #category : 'accessing' }
AutomaticRewriting >> condition: aBlock [
	condition := aBlock
]

{ #category : 'accessing' }
AutomaticRewriting >> context: aContext [
	context := aContext
]

{ #category : 'accessing' }
AutomaticRewriting >> contextOfDeprecatedMethod [
	^context
]

{ #category : 'accessing' }
AutomaticRewriting >> contextOfSender [
	^context home sender
]

{ #category : 'compatibility period' }
AutomaticRewriting >> currentMajorVersion [

	^ SystemVersion current major
]

{ #category : 'accessing' }
AutomaticRewriting >> date: aDate [
	deprecationDate := aDate
]

{ #category : 'handling' }
AutomaticRewriting >> defaultAction [
	"Record the deprecation in the log that #deprecationsWhile: reads, react as the settings say, and rewrite the caller if the settings say so."

	RecursionStopper during: [
		self class addLog: self.
		self class reaction react: self.
		self shouldTransform ifTrue: [ self transform ] ]
]

{ #category : 'compatibility period' }
AutomaticRewriting >> deprecatedMethod [

	^ self contextOfDeprecatedMethod homeMethod
]

{ #category : 'private' }
AutomaticRewriting >> deprecatedMethodName [
	^self contextOfDeprecatedMethod homeMethod printString
]

{ #category : 'accessing' }
AutomaticRewriting >> deprecationDate [

	^ deprecationDate ifNil: [ 'unknown' ]
]

{ #category : 'compatibility period' }
AutomaticRewriting >> deprecationMajorVersion [
	"Answer the major Pharo version given with #in:, such as #Pharo14 or 'pharo13', or nil when no Pharo version was given. Deprecation belongs to the kernel, which has no regular expressions."

	| string stream digits |
	versionString ifNil: [ ^ nil ].
	string := versionString asString asLowercase.
	(string includesSubstring: 'pharo') ifFalse: [ ^ nil ].
	stream := ReadStream on: string.
	stream upToAll: 'pharo'.
	stream skipSeparators.
	digits := String streamContents: [ :out |
		          [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ out nextPut: stream next ] ].
	digits ifEmpty: [ ^ nil ].
	^ digits asNumber
]

{ #category : 'accessing' }
AutomaticRewriting >> explanation: aString [
	explanationString := aString
]

{ #category : 'accessing' }
AutomaticRewriting >> explanationString [

	^ explanationString
]

{ #category : 'comparing' }
AutomaticRewriting >> hash [
	^(context ifNil: [explanationString]) hash
]

{ #category : 'compatibility period' }
AutomaticRewriting >> isAtEndOfCompatibilityPeriod [
	"PHEP-0004: whether this deprecation states the Pharo version that introduced it and the current version is the last of its compatibility period or later."

	| deprecationVersion |
	deprecationVersion := self deprecationMajorVersion ifNil: [ ^ false ].
	^ self currentMajorVersion >= (deprecationVersion + self compatibilityPeriod - 1)
]

{ #category : 'compatibility period' }
AutomaticRewriting >> isDeprecatedPackageName: aString [
	"Whether aString names a package of deprecated methods of one Pharo version, such as Deprecated14 or Deprecated14-Kernel."

	^ (aString beginsWith: 'Deprecated') and: [ aString size > 10 and: [ (aString at: 11) isDigit ] ]
]

{ #category : 'compatibility period' }
AutomaticRewriting >> isKernel [
	"PHEP-0004: whether the deprecated API belongs to the kernel, decided by the package of the deprecated method, not of its class: an extension method belongs to the package that extends the class."

	^ self isKernelPackageNamed: self deprecatedMethod package name
]

{ #category : 'compatibility period' }
AutomaticRewriting >> isKernelPackageNamed: aString [
	"A method moved to a DeprecatedN package keeps its kind in the name of that package: DeprecatedN-Kernel holds kernel methods, DeprecatedN the others. Without the bootstrap baseline, assume kernel, which keeps the longer period."

	| baseline |
	(self isDeprecatedPackageName: aString) ifTrue: [ ^ aString endsWith: '-Kernel' ].
	
	"Look for the baseline class using reflection in case it's not loaded..."
	baseline := self class environment
		at: #BaselineOfPharoBootstrap
		ifAbsent: [ ^ true ].

	^ baseline kernelPackageNames includes: aString
]

{ #category : 'private' }
AutomaticRewriting >> rewriterClass [

	^ OCParseTreeRewriter
]

{ #category : 'accessing' }
AutomaticRewriting >> rule: aRule [
	rule := aRule
]

{ #category : 'private' }
AutomaticRewriting >> sendingMethodName [
	^self contextOfSender homeMethod printString
]

{ #category : 'handling' }
AutomaticRewriting >> shouldTransform [

	rule ifNil: [ ^ false ].
	(condition isNil or: [ condition cull: self ]) ifFalse: [ ^ false ].
	^ self class rewriting shouldRewrite: self
]

{ #category : 'signaling' }
AutomaticRewriting >> signal [
	| pragma homeMethod |
	homeMethod := context homeMethod.
	(homeMethod hasPragmaNamed: #transform:to:) ifFalse: [ ^super signal ].

	pragma := homeMethod pragmaAt: #transform:to:.
	self rule: pragma arguments first -> pragma arguments second.
	self transform
]

{ #category : 'reacting' }
AutomaticRewriting >> signalError [
	"Signal an error for this deprecation, so that running deprecated code fails."

	Error signal: self messageText
]

{ #category : 'accessing' }
AutomaticRewriting >> version: aString [
	versionString := aString
]

{ #category : 'accessing' }
AutomaticRewriting >> versionString [

	^ versionString ifNil: [ 'unknown' ]
]

{ #category : 'reacting' }
AutomaticRewriting >> warn [
	"Signal this deprecation as an unhandled warning: a blocking dialog in an interactive image."

	super defaultAction
]
