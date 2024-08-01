"
A MCPackageLoader is responsible for loading packages.  It gets used by VersionLoader, so it is eventually responsible for loading everything.

Instance Variables
	additions:		<Definitions>  Definitions that need to be added
	errorDefinitions:		<Object>
	obsoletions:		<Object>
	provisions:		<Object>
	removals:		<Object>
	requirements:		<Object>
	unloadableDefinitions:		<Object>
"
Class {
	#name : 'MCPackageLoader',
	#superclass : 'Object',
	#instVars : [
		'requirements',
		'unloadableDefinitions',
		'obsoletions',
		'additions',
		'removals',
		'provisions'
	],
	#category : 'Monticello-Loading',
	#package : 'Monticello',
	#tag : 'Loading'
}

{ #category : 'public' }
MCPackageLoader class >> installSnapshot: aSnapshot [
	self new
		installSnapshot: aSnapshot;
		load
]

{ #category : 'public' }
MCPackageLoader class >> unloadPackage: aPackage [

	self new unloadPackage: aPackage
]

{ #category : 'public' }
MCPackageLoader class >> updatePackage: aPackage withSnapshot: aSnapshot [
	self new
		updatePackage: aPackage withSnapshot: aSnapshot;
		load
]

{ #category : 'patch ops' }
MCPackageLoader >> addDefinition: aDefinition [
	additions add: aDefinition
]

{ #category : 'private' }
MCPackageLoader >> analyze [

	| sorter |
	sorter := self sorterForItems: additions.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadableDefinitions := sorter itemsWithMissingRequirements asSortedCollection.

	sorter := self sorterForItems: removals.
	removals := sorter orderedItems reversed
]

{ #category : 'private' }
MCPackageLoader >> basicLoad [

	SourceFiles deferFlushDuring: [ self basicLoadDefinitions ]
]

{ #category : 'private' }
MCPackageLoader >> basicLoadDefinitions [

	self loadClassDefinitions.

	"We want to announce the addition of the methods only once all methods are loaded because the system might react to the annoucements and execute code requirement other methods from the classes we are loading."
	self class codeChangeAnnouncer delayAnnouncementsAfter: [ additions select: [ :aDefinition | aDefinition isMethodDefinition ] thenDo: [ :definition | definition load ] ].

	removals do: [ :each | each unload ] displayingProgress: 'Cleaning up...'.

	additions do: [ :each | each postloadOver: (self obsoletionFor: each) ] displayingProgress: 'Initializing...'
]

{ #category : 'private' }
MCPackageLoader >> dependencyWarning [

	| packageName |
	packageName := additions
		               detect: [ :aDefinition | aDefinition isClassDefinition ]
		               ifFound: [ :aDefinition | aDefinition packageName ]
		               ifNone: [ 'UNKNOWN' ].

	^ String streamContents: [ :stream |
		  stream
			  nextPutAll: 'Package ';
			  nextPutAll: packageName;
			  nextPutAll: ' depends on the following classes:';
			  cr.
		  requirements do: [ :each |
			  stream
				  space;
				  space;
				  nextPutAll: each;
				  cr ].
		  stream
			  nextPutAll: 'You must resolve these dependencies before you will be able to load these definitions: ';
			  cr.
		  unloadableDefinitions do: [ :ea |
			  stream
				  space;
				  space;
				  nextPutAll: ea summary;
				  cr ] ]
]

{ #category : 'initialization' }
MCPackageLoader >> initialize [

	super initialize.
	additions := OrderedCollection new.
	removals := OrderedCollection new.
	obsoletions := Dictionary new
]

{ #category : 'public' }
MCPackageLoader >> installSnapshot: aSnapshot [
	| patch |
	patch := aSnapshot patchRelativeToBase: MCSnapshot empty.
	patch applyTo: self.

]

{ #category : 'public' }
MCPackageLoader >> load [

	self validate.
	self basicLoad
]

{ #category : 'private' }
MCPackageLoader >> loadClassDefinitions [
	"Here we are loading the class definitions.
	
	When loading a class it is possible that it fails. The reason is that a class can use a trait from this package, but the dependency sorter of Monticello does not sort those traits before the classes using it.
	In that case we are implementing a retry mechanism. If error persist, the exception message is logged to the Transcript"

	| errors |
	errors := Dictionary new.

	"We first load the class definitions and store the potential errors"
	[
	(additions select: [ :aDefinition | aDefinition isClassDefinition ])
		do: [ :aDefinition |
			[ aDefinition load ]
				on: Error
				do: [ :error | errors at: aDefinition put: error ] ]
		displayingProgress: 'Loading classes...'.

	"Until we succed to fix errors, we try to install them"
	[
	| previousErrors |
	previousErrors := errors copy.
	previousErrors
		keysDo: [ :aClassDefinition |
			errors removeKey: aClassDefinition.
			[ aClassDefinition load ]
				on: Error
				do: [ :error | errors at: aClassDefinition put: error ] ].
	previousErrors size = errors size ] whileFalse ] ensure: [ "Finally we warn about the errors we could not fix."
		errors ifNotEmpty: [
			self warnAboutErrors: errors ] ]
]

{ #category : 'patch ops' }
MCPackageLoader >> modifyDefinition: old to: new [
	self addDefinition: new.
	obsoletions at: new put: old.
]

{ #category : 'private' }
MCPackageLoader >> obsoletionFor: aDefinition [
	^ obsoletions at: aDefinition ifAbsent: [nil]
]

{ #category : 'private' }
MCPackageLoader >> orderedAdditions [
	^ additions
]

{ #category : 'private' }
MCPackageLoader >> provisions [
	^ provisions ifNil: [provisions := Set withAll: Smalltalk globals keys]
]

{ #category : 'patch ops' }
MCPackageLoader >> removeDefinition: aDefinition [

	removals add: aDefinition
]

{ #category : 'private' }
MCPackageLoader >> sorterForItems: aCollection [
	| sorter |
	sorter := MCDependencySorter items: aCollection.
	sorter addExternalProvisions: self provisions.
	^ sorter
]

{ #category : 'public' }
MCPackageLoader >> unloadPackage: aPackage [

	self updatePackage: aPackage withSnapshot: MCSnapshot empty
]

{ #category : 'public' }
MCPackageLoader >> updatePackage: aPackage withSnapshot: aSnapshot [
	|  patch packageSnap |
	packageSnap := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: packageSnap.
	patch applyTo: self.
	packageSnap definitions do: [:ea | self provisions addAll: ea provisions]

]

{ #category : 'private' }
MCPackageLoader >> validate [
	self analyze.
	unloadableDefinitions isEmpty ifFalse: [self warnAboutDependencies].
]

{ #category : 'private' }
MCPackageLoader >> warnAboutDependencies [ 
	self notify: self dependencyWarning
]

{ #category : 'private' }
MCPackageLoader >> warnAboutErrors: errorsDict [

	self notify: (String streamContents: [ :s |
			 s
				 nextPutAll: 'The following definitions had errors while loading.  Press Proceed to try to load them again (they may work on a second pass):';
				 cr.
			 errorsDict keysDo: [ :ea |
				 s
					 space;
					 space;
					 nextPutAll: ea summary;
					 nextPutAll: ' (';
					 nextPutAll: ((errorsDict at: ea) description asString);
					 nextPutAll: ')';
					 cr ] ])
]
