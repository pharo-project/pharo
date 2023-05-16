"
ChangeSets keep track of the changes made to a system, so they can be written on a file as source code (a ""fileOut""). 

---
preamble and postscript:  two strings that serve as prefix (useful for documentation) and suffix (useful for doits) to the fileout of the changeSet.


changeRecords -  Dictionary {class name -> a ClassChangeRecord}.
These classChangeRecords (qv) remember all of the system changes.
"
Class {
	#name : #ChangeSet,
	#superclass : #Object,
	#instVars : [
		'name',
		'preamble',
		'postscript',
		'changeRecords'
	],
	#classVars : [
		'AllChangeSets',
		'DefaultChangeSetDirectoryName',
		'MustCheckForSlips'
	],
	#classInstVars : [
		'current'
	],
	#category : #'System-Changes-Base'
}

{ #category : #enumerating }
ChangeSet class >> allChangeSetNames [

	^ self allChangeSets collect: [:c | c name]
]

{ #category : #enumerating }
ChangeSet class >> allChangeSets [
	"Return the list of all current ChangeSets"

	^ AllChangeSets
]

{ #category : #enumerating }
ChangeSet class >> allChangeSets: aCollection [
	"Return the list of all current ChangeSets"

	AllChangeSets := aCollection
]

{ #category : #enumerating }
ChangeSet class >> allChangeSetsWithClass: class selector: selector [

	class ifNil: [^ #()].
	^ self allChangeSets select:
		[:cs | (cs atSelector: selector class: class) ~~ #none]
]

{ #category : #services }
ChangeSet class >> assuredChangeSetNamed: aName [
	"Answer a change set of the given name.  If one already exists, answer that, else create a new one and answer it."

	^ (self named: aName)
		  ifNotNil: [ :existing | existing ]
		  ifNil: [ self newChangeSet: aName ]
]

{ #category : #enumerating }
ChangeSet class >> changeSetsNamedSuchThat: nameBlock [

	"(ChangeSet changeSetsNamedSuchThat:
		[:name | name first isDigit and: [name initialInteger >= 373]])
		do: [:cs | AllChangeSets remove: cs wither]"

	^ AllChangeSets select: [:aChangeSet | nameBlock value: aChangeSet name]
]

{ #category : #'file in/out' }
ChangeSet class >> classesOrder: classes [
	"Answer a collection with the classes ordered so they can be filed in."

	| listInOrder |
	"SharedPools need to go first"
	listInOrder := Class superclassOrder: (classes select: [ :each | each inheritsFrom: SharedPool ]).
	listInOrder addAll: (Class superclassOrder: (classes reject: [ :each | each inheritsFrom: SharedPool ])).
	^ listInOrder
]

{ #category : #cleanup }
ChangeSet class >> cleanUp: aggressive [
	"Only delete change sets when being aggressive"

	aggressive
		ifFalse: [ ^ self ].
	self removeChangeSetsNamedSuchThat: [ :each | true ].
	self resetCurrentToNewUnnamedChangeSet
]

{ #category : #services }
ChangeSet class >> countOfChangeSetsWithClass: aClass andSelector: aSelector [
	"Answer how many change sets record a change for the given class and selector"

	^ (self allChangeSetsWithClass: aClass selector: aSelector) size
]

{ #category : #'current changeset' }
ChangeSet class >> current [
	"Return the current changeset assure first that we have a named changeset."

	(current isNil or: [ current isMoribund ]) ifTrue: [
		self
			newChanges: (self assuredChangeSetNamed: 'Unnamed')
			withOld: current ].
	^ current
]

{ #category : #defaults }
ChangeSet class >> defaultChangeSetDirectory [
	"Answer the directory in which to store ChangeSets.
	Answer the default directory if the preferred directory doesn't exist."
	| dir directoryName |
	directoryName := self defaultChangeSetDirectoryName.
	dir := directoryName isEmptyOrNil
		ifTrue: [ FileSystem workingDirectory ]
		ifFalse: [ FileSystem workingDirectory / directoryName ].
	dir exists
		ifTrue: [^ dir].
	^ FileSystem workingDirectory
]

{ #category : #defaults }
ChangeSet class >> defaultChangeSetDirectory: dirOrName [
	"Set the Preference for storing change sets to the given directory or name (possibly relative).
	Rewrite directory names below the default directory as relative names.
	If dirOrName is an empty string, use the default directory."

	"ChangeSet defaultChangeSetDirectory: 'changeSets'"

	| dirName defaultFullName |

	dirName := dirOrName isString
				ifTrue: [ dirOrName asFileReference fullName ]
				ifFalse: [dirOrName fullName].

	defaultFullName := FileSystem workingDirectory fullName.
	dirName = defaultFullName
		ifTrue: [dirName := '']
		ifFalse: [
			(dirName beginsWith: defaultFullName , FileSystem disk delimiter asString)
				ifTrue: [dirName := dirName copyFrom: defaultFullName size + 2 to: dirName size]].
	self defaultChangeSetDirectoryName: dirName
]

{ #category : #settings }
ChangeSet class >> defaultChangeSetDirectoryName [

	^ DefaultChangeSetDirectoryName ifNil: [
		  DefaultChangeSetDirectoryName := FileSystem workingDirectory ]
]

{ #category : #settings }
ChangeSet class >> defaultChangeSetDirectoryName: aName [

	DefaultChangeSetDirectoryName := aName
]

{ #category : #defaults }
ChangeSet class >> defaultName [

	^ self uniqueNameLike: 'Unnamed'
]

{ #category : #services }
ChangeSet class >> doesAnyChangeSetHaveClass: aClass andSelector: aSelector [
	"Answer whether any known change set bears a change for the given class and selector"

	^ (self countOfChangeSetsWithClass: aClass andSelector: aSelector) > 0
]

{ #category : #'file in/out' }
ChangeSet class >> fileOutChangeSetsNamed: nameList [
	"File out the list of change sets whose names are provided"
     "ChangesOrganizer fileOutChangeSetsNamed: #('New Changes' 'miscTidies-sw')"

	| notFound empty infoString |
	notFound := OrderedCollection new.
	empty := OrderedCollection new.
	nameList do:
		[:aName | | aChangeSet |
			(aChangeSet := self named: aName)
				ifNotNil: [aChangeSet isEmpty
							ifTrue: [empty add: aName]
							ifFalse: [aChangeSet fileOut]]
				ifNil: [notFound add: aName]].

	infoString := (nameList size - notFound size) printString, ' change set(s) filed out'.
	notFound notEmpty ifTrue:
		[infoString := infoString, '

', notFound size printString, ' change set(s) not found:'.
		notFound do:
			[:aName | infoString := infoString, '
', aName]].
	empty notEmpty ifTrue:
		[infoString := infoString, '
', empty size printString, ' change set(s) were empty:'.
		empty do:
			[:aName | infoString := infoString, '
', aName]].

	self inform: infoString
]

{ #category : #enumerating }
ChangeSet class >> gatherChangeSets [
	"Collect any change sets created in other projects"
	<script>

	| allChangeSets |
	allChangeSets := AllChangeSets asSet.
	self allSubInstances do: [:each | | obsolete |
		(allChangeSets includes: each) == (obsolete := each isMoribund) ifTrue:[
			obsolete
				ifTrue: ["Was included and is obsolete."
						AllChangeSets remove: each]
				ifFalse: ["Was not included and is not obsolete."
						AllChangeSets add: each]]].
	^ AllChangeSets
]

{ #category : #'file in/out' }
ChangeSet class >> hasNoDependenciesFor: aTrait in: traits [
	"Answer if the trait does not depend on a trait in the collection."

	^ traits allSatisfy: [ :another |
		aTrait == another or: [ "are the same"
			aTrait ~= another classSide and: [ "is not the classTrait of another"
				(aTrait traitComposition allTraits includes: another) not ] ] ]
]

{ #category : #'class initialization' }
ChangeSet class >> initialize [

	AllChangeSets ifNil: [ AllChangeSets := OrderedCollection new ].
	self defaultChangeSetDirectoryName: nil.
	self gatherChangeSets
]

{ #category : #enumerating }
ChangeSet class >> mostRecentChangeSetWithChangeForClass: class selector: selector [

	| hits |
	hits := self allChangeSets select:
		[:cs | (cs atSelector: selector class: class) ~~ #none].
	hits isEmpty ifTrue: [^ 'not in any change set'].
	^ 'recent cs: ', hits last name
]

{ #category : #settings }
ChangeSet class >> mustCheckForSlips [

	^ MustCheckForSlips ifNil: [ MustCheckForSlips := true ]
]

{ #category : #settings }
ChangeSet class >> mustCheckForSlips: aBoolean [

	MustCheckForSlips := aBoolean
]

{ #category : #enumerating }
ChangeSet class >> named: aName [
	"Return the change set of the given name, or nil if none found."

	^ AllChangeSets
		  detect: [ :aChangeSet | aChangeSet name = aName ]
		  ifNone: [ nil ]
]

{ #category : #'instance creation' }
ChangeSet class >> new [
	"All current changeSets must be registered in the AllChangeSets collection.
	Due to a quirk of history, this is maintained as class variable of ChangeSorter."

	^ self newChangeSet: self defaultName
]

{ #category : #services }
ChangeSet class >> newChangeSet [
	"Prompt the user for a name, and establish a new change set of
	that name (if ok), making it the current changeset.  Return nil
	of not ok, else return the actual changeset."

	| newName newSet |
	newName := UIManager default request: 'Please name the new change set:' initialAnswer: self defaultName.
	newName isEmptyOrNil ifTrue: [ ^ nil ].
	newSet := self newChangeSet: newName.
	newSet ifNotNil: [ self newChanges: newSet ].
	^ newSet
]

{ #category : #services }
ChangeSet class >> newChangeSet: newName [

	| newSet |
	newName ifNil: [ ^ nil ].
	(self named: newName) ifNotNil: [
		self inform: 'Sorry that name is already used'.
		^ nil ].
	newSet := self newNamed: newName.
	AllChangeSets add: newSet.
	^ newSet
]

{ #category : #'current changeset' }
ChangeSet class >> newChanges: aChangeSet [
	"Set the system ChangeSet to be the argument, aChangeSet."

	"Pay attention not to use newChanges in current and other methods to avoid infinite cycles."

	self newChanges: aChangeSet withOld: self current
]

{ #category : #'current changeset' }
ChangeSet class >> newChanges: aChangeSet withOld: old [
	"Set the system ChangeSet to be the argument, aChangeSet."

	SystemAnnouncer uniqueInstance unsubscribe: old.
	current := aChangeSet.

	SystemAnnouncer uniqueInstance private weak
		when: ClassRemoved send: #classRemoved: to: aChangeSet;
		when: ClassAdded send: #classAdded: to: aChangeSet;
		when: ClassCommented send: #classCommented: to: aChangeSet;
		when: ClassRenamed send: #classRenamed: to: aChangeSet;
		when: ClassReorganized send: #classReorganized: to: aChangeSet;
		when: ClassRecategorized send: #classRecategorized: to: aChangeSet;
		when: ClassModifiedClassDefinition send: #classModified: to: aChangeSet.

	SystemAnnouncer uniqueInstance private weak
		when: MethodAdded send: #methodAdded: to: aChangeSet;
		when: MethodModified send: #methodModified: to: aChangeSet;
		when: MethodRemoved send: #methodRemoved: to: aChangeSet;
		when: MethodRecategorized send: #methodRecategorized: to: aChangeSet;
		when: DoItChunkImported send: #doItChunkImported: to: aChangeSet.

	SystemAnnouncer announce: (CurrentChangeSetChanged new old: old; new: aChangeSet ; yourself)
]

{ #category : #services }
ChangeSet class >> newChangesFromStream: aStream named: aName [
	"File in the code from the stream into a new change set whose
	name is derived from aName. Leave the 'current change set'
	unchanged. Return the new change set or nil on failure."

	| oldChanges newName newSet |
	oldChanges := self current.
	newName := aName withoutPeriodSuffix.
	newSet := self newChangeSet: newName.
	[
	newSet ifNotNil: [
		self newChanges: newSet.
		CodeImporter evaluateReadStream: aStream ] ] ensure: [ self newChanges: oldChanges ].
	^ newSet
]

{ #category : #'instance creation' }
ChangeSet class >> newNamed: aName [

	^ (self basicNew name: aName) initialize
]

{ #category : #enumerating }
ChangeSet class >> promoteToTop: aChangeSet [
	"Make aChangeSet the first in the list from now on"

	AllChangeSets remove: aChangeSet ifAbsent: [ ^ self ].
	AllChangeSets add: aChangeSet
]

{ #category : #defaults }
ChangeSet class >> promptForDefaultChangeSetDirectoryIfNecessary [
	<script>

	| path |
	path := UIManager default
		chooseDirectory: 'Please select where you want to save your cs'
		from: FileSystem workingDirectory.
	self defaultChangeSetDirectoryName: path asFileReference.
	^ path asFileReference
]

{ #category : #'system-events' }
ChangeSet class >> registerInterestToSystemAnnouncer [
	<systemEventRegistration>

	self newChanges: self current
]

{ #category : #enumerating }
ChangeSet class >> removeChangeSet: aChangeSet [
	"Remove the given changeSet.  Caller must assure that it's cool to do this"

	AllChangeSets remove: aChangeSet ifAbsent: [].
	aChangeSet wither
]

{ #category : #enumerating }
ChangeSet class >> removeChangeSetsNamedSuchThat: nameBlock [

	(self changeSetsNamedSuchThat: nameBlock) do: [ :cs |
		self removeChangeSet: cs ]
]

{ #category : #services }
ChangeSet class >> reorderChangeSets [
	"Change the order of the change sets to something more convenient:
		First come all numbered updates.
		Next come all remaining changesets"
	<script>

	| newMid newTail |
	newMid := OrderedCollection new.
	newTail := OrderedCollection new.
	self allChangeSets do:
		[:aChangeSet |
			aChangeSet isNumbered
						ifTrue: [newMid add: aChangeSet]
						ifFalse: [newTail add: aChangeSet]].
	self allChangeSets: newMid, newTail
]

{ #category : #'current changeset' }
ChangeSet class >> resetCurrentToNewUnnamedChangeSet [

	current := self new.
	self newChanges: current
]

{ #category : #'file in/out' }
ChangeSet class >> traitsOrder: aCollection [
	"Arrange the traits in the collection, first who don't depend on others."

	| unprocessed |
	unprocessed := aCollection asSet.
	^ Array new: unprocessed size streamContents: [ :stream |
		unprocessed size timesRepeat: [
			| aTrait |
			aTrait := unprocessed detect: [ :each | self hasNoDependenciesFor: each in: unprocessed ].
			stream nextPut: aTrait.
			unprocessed remove: aTrait] ]
]

{ #category : #defaults }
ChangeSet class >> uniqueNameLike: aString [

	| try index |
	(self named: aString)
		ifNil: [ ^ aString ].
	index := 1.
	[ try := aString , index printString.
	  (self named: try)
		ifNil: [ ^ try ].
	  index := index + 1 ] repeat
]

{ #category : #'moving changes' }
ChangeSet >> absorbClass: className from: otherChangeSet [
	"Absorb into the receiver all the changes found in the class in the other change set.
	*** Classes renamed in otherChangeSet may have problems"

	(self changeRecorderFor: className) assimilateAllChangesIn:
		(otherChangeSet changeRecorderFor: className)
]

{ #category : #'moving changes' }
ChangeSet >> absorbMethod: selector class: aClass from: aChangeSet [
	"Absorb into the receiver all the changes for the method in the class in the other change set."

	| info |
	info := aChangeSet methodChanges
		        at: aClass name
		        ifAbsent: [ Dictionary new ].
	self atSelector: selector class: aClass put: (info at: selector)
]

{ #category : #'change logging' }
ChangeSet >> addClass: class [
	"Include indication that a new class was created."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #new.
	self atClass: class add: #change.
	self addCoherency: class name
]

{ #category : #private }
ChangeSet >> addCoherency: className [
	"SqR! 19980923: If I recreate the class then don't remove it"

	(self changeRecorderFor: className)
		checkCoherence.
"
	classRemoves remove: className ifAbsent: [].
	(classChanges includesKey: className) ifTrue:
		[(classChanges at: className) remove: #remove ifAbsent: []]
"
]

{ #category : #'method changes' }
ChangeSet >> adoptSelector: aSelector forClass: aClass [
	"Adopt the given selector/class combination as a change in the receiver"

	self noteNewMethod: (aClass compiledMethodAt: aSelector)
			forClass: aClass selector: aSelector priorMethod: nil
]

{ #category : #'moving changes' }
ChangeSet >> assimilateAllChangesFoundIn: otherChangeSet [
	"Make all changes in otherChangeSet take effect on self as if they happened just now."

	otherChangeSet changedClassNames do:
		[:className | self absorbClass: className from: otherChangeSet]
]

{ #category : #'file in/out' }
ChangeSet >> assurePostscriptExists [
	"Make sure there is a StringHolder holding the postscript.  "

	"NOTE: FileIn recognizes the postscript by the line with Postscript: on it"

	postscript ifNil: [
			postscript := '"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!).
Be sure to put any further comments in double-quotes, like this one."
' ]
]

{ #category : #'file in/out' }
ChangeSet >> assurePreambleExists [
	"Make sure there is a StringHolder holding the preamble; if it's found to have reverted to empty contents, put up the template"

	preamble isEmptyOrNil ifTrue: [ preamble := self preambleTemplate ]
]

{ #category : #private }
ChangeSet >> atClass: class add: changeType [

	(self changeRecorderFor: class)
		noteChangeType: changeType fromClass: class
]

{ #category : #private }
ChangeSet >> atClass: class includes: changeType [

	^ (changeRecords at: class name ifAbsent: [ ^ false ])
		  includesChangeType: changeType
]

{ #category : #private }
ChangeSet >> atSelector: selector class: class [

	^ (changeRecords at: class name ifAbsent: [ ^ #none ])
		  atSelector: selector
		  ifAbsent: [ ^ #none ]
]

{ #category : #'method changes' }
ChangeSet >> atSelector: selector class: class put: changeType [

	(self changeRecorderFor: class) atSelector: selector put: changeType
]

{ #category : #accessing }
ChangeSet >> author [

	| author |
	self assurePreambleExists.
	author := self preambleString lineNumber: 3.
	author := author copyFrom: 8 to: author size. "Strip the 'Author:' prefix. Ugly ugly."
	^ author trimBoth
]

{ #category : #'change logging' }
ChangeSet >> changeClass: class from: oldClass [
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	class wantsChangeSetLogging ifFalse: [^ self].
	class isMeta
		ifFalse: [self atClass: class add: #change]	"normal"
		ifTrue: [((self classChangeAt: class instanceSide name) includes: #add)
			ifTrue: [self atClass: class add: #add] 	"When a class is defined, the metaclass
				is not recorded, even though it was added.  A further change is
				really just part of the original add."
			ifFalse: [self atClass: class add: #change]].
	self addCoherency: class name.
	(self changeRecorderFor: class) notePriorDefinition: oldClass
]

{ #category : #private }
ChangeSet >> changeRecorderFor: class [

	| cname |
	cname := class isString
		ifTrue: [ class ]
		ifFalse: [ class isBehavior ifTrue: [class name] ifFalse: [ nil printString] ].
	^ changeRecords at: cname ifAbsentPut: [ ClassChangeRecord new initFor: cname ]
]

{ #category : #'class changes' }
ChangeSet >> changedClassNames [
	"Answer a OrderedCollection of the names of changed or edited classes.
	Does include removed classes.  Sort alphabetically."

	^ changeRecords keysSortedSafely
]

{ #category : #'class changes' }
ChangeSet >> changedClasses [
	"Answer an OrderedCollection of changed or edited classes.
	Does not include removed classes.  Sort alphabetically by name."

	"Much faster to sort names first, then convert back to classes.  Because metaclasses reconstruct their name at every comparison in the sorted collection.
	8/91 sw chgd to filter out non-existent classes (triggered by problems with class-renames"

	^ self changedClassNames
		collect: [ :className | Smalltalk globals classNamed: className ]
		thenSelect: [ :aClass | aClass notNil ]
]

{ #category : #'method changes' }
ChangeSet >> changedMessageList [
	"Used by a message set browser to access the list view information."

	| messageList |
	messageList := OrderedCollection new.
	changeRecords associationsDo: [:clAssoc | | className classIsMeta |
		className := clAssoc key asSymbol.
		classIsMeta := (className findTokens: ' ') size > 1.

		(clAssoc value allChangeTypes includes: #comment) ifTrue:
			[messageList add:
				(RGCommentDefinition new
					parentName: className) asActive ].

		clAssoc value methodChangeTypes associationsDo: [:mAssoc |
			(#(remove addedThenRemoved) includes: mAssoc value) ifFalse:
				[messageList add:
					((RGMethodDefinition named: mAssoc key)
						parentName: className;
						isMetaSide: classIsMeta) asActive ]]].
	^ messageList asArray sort
]

{ #category : #'file in/out' }
ChangeSet >> checkForSlips [
	"Return a collection of method refs with possible debugging code in them."

	| slips |
	slips := OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc |
			(#( remove addedThenRemoved ) includes: mAssoc value) ifFalse: [
				aClass compiledMethodAt: mAssoc key ifPresent: [ :method | (self hasReportableSlip: method) ifTrue: [ slips add: aClass >> mAssoc key ] ] ] ] ].
	^ slips
]

{ #category : #'change logging' }
ChangeSet >> classAdded: anEvent [

	self addClass: anEvent classAdded
]

{ #category : #'class changes' }
ChangeSet >> classChangeAt: className [
	"Return what we know about class changes to this class."

	^ (changeRecords at: className ifAbsent: [ ^ Set new ])
		  allChangeTypes
]

{ #category : #'change logging' }
ChangeSet >> classCommented: anEvent [

	self commentClass: anEvent classCommented
]

{ #category : #'change logging' }
ChangeSet >> classModified: anEvent [

	self
		changeClass: anEvent newClassDefinition
		from: anEvent oldClassDefinition
]

{ #category : #'change logging' }
ChangeSet >> classRecategorized: anEvent [

	self
		changeClass: anEvent classRecategorized
		from: anEvent classRecategorized
]

{ #category : #'change logging' }
ChangeSet >> classRemoved: anEvent [

	self noteRemovalOf: anEvent classRemoved
]

{ #category : #accessing }
ChangeSet >> classRemoves [

	^ changeRecords keys select: [ :className |
		  (changeRecords at: className) isClassRemoval ]
]

{ #category : #'change logging' }
ChangeSet >> classRenamed: anEvent [

	self
		renameClass: anEvent classRenamed
		from: anEvent oldName
		to: anEvent newName
]

{ #category : #'change logging' }
ChangeSet >> classReorganized: anEvent [

	self reorganizeClass: anEvent classReorganized
]

{ #category : #initialization }
ChangeSet >> clear [

	"Reset the receiver to be empty."

	changeRecords := Dictionary new.
	preamble := nil.
	postscript := nil
]

{ #category : #'class changes' }
ChangeSet >> commentClass: class [
	"Include indication that a class comment has been changed."

	class wantsChangeSetLogging ifFalse: [ ^ self ].
	self atClass: class add: #comment
]

{ #category : #'class changes' }
ChangeSet >> containsClass: aClass [

	^ self changedClasses includes: aClass
]

{ #category : #'file in/out' }
ChangeSet >> defaultChangeSetDirectory [

	^ self class defaultChangeSetDirectory
]

{ #category : #'change logging' }
ChangeSet >> doItChunkImported: anAnnouncemnet [

	self class current = self ifFalse: [ ^ self ].

	"Parse preamble and postcript as a do it."

	((anAnnouncemnet contents beginsWith: '"Change Set:') and: [ self preambleString isNil ])
		ifTrue: [ self preambleString: anAnnouncemnet contents ].
	((anAnnouncemnet contents beginsWith: '"Postscript:') and: [ self postscriptString isNil ])
		ifTrue: [ self postscriptString: anAnnouncemnet contents ]
]

{ #category : #accessing }
ChangeSet >> editPostscript [
	"Edit the receiver's postscript, in a separate window."

	self assurePostscriptExists.
	UIManager default
		edit: self postscript
		label: 'Postscript for ChangeSet named ', name
		accept:[:aString| self postscript: aString]
]

{ #category : #'moving changes' }
ChangeSet >> editPreamble [
	"Edit the receiver's preamble, in a separate window."

	self assurePreambleExists.
	UIManager default
		edit: self preamble
		label: 'Preamble for ChangeSet named ', name
		accept:[:aString| self preamble: aString]
]

{ #category : #'moving changes' }
ChangeSet >> expungeEmptyClassChangeEntries [

	changeRecords keysAndValuesRemove: [ :className :classRecord |
		classRecord hasNoChanges ]
]

{ #category : #'file in/out' }
ChangeSet >> fileOut [
	"File out the receiver, to a file whose name is a function of the
	change-set name and a unique numeric tag."

	| fileReference dir |
	dir := self class promptForDefaultChangeSetDirectoryIfNecessary.
	fileReference := ( dir / self name , 'cs') nextVersion.
	self fileOutBody: fileReference
]

{ #category : #'file in/out' }
ChangeSet >> fileOutBody: fileReference [

	UIManager default
		showWaitCursorWhile:
			[
			| internalStream |
			internalStream := (String new: 10000) writeStream.
			internalStream
				header;
				timeStamp.
			self fileOutPreambleOn: internalStream.
			self fileOutOn: internalStream.
			self fileOutPostscriptOn: internalStream.

			CodeExporter
				writeSourceCodeFrom: internalStream
				toFileReference: fileReference ]
]

{ #category : #'file in/out' }
ChangeSet >> fileOutChangesFor: class on: stream [
	"Write out all the method changes for this class."

	| changes |
	changes := Set new.
	(self methodChangesAtClass: class name)
		associationsDo: [ :mAssoc |
			(mAssoc value = #remove or: [ mAssoc value = #addedThenRemoved ])
				ifFalse: [ changes add: mAssoc key ] ].
	changes isEmpty
		ifTrue: [ ^ self ].
	class fileOutChangedMessages: changes on: stream.
	stream cr
]

{ #category : #private }
ChangeSet >> fileOutClassDefinition: class on: stream [
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename)
		ifTrue: [
			stream
				nextChunkPut: 'Smalltalk renameClassNamed: #' , (self oldNameFor: class) , ' as: #' , class name;
				cr ].
	(self atClass: class includes: #change)
		ifTrue: [
			"fat definition only needed for changes"
			stream
				nextChunkPut: class definitionString;
				cr.
			DeepCopier new checkClass: class	"If veryDeepCopy weakly copies some inst
			vars in this class, warn author when new ones are added." ]
		ifFalse: [
			(self atClass: class includes: #add)
				ifTrue: [
					"use current definition for add"
					stream
						nextChunkPut: class definition;
						cr.
					DeepCopier new checkClass: class	"If veryDeepCopy weakly copies some inst
				vars in this class, warn author when new ones are added." ] ].
	(self atClass: class includes: #comment)
		ifFalse: [ ^ self ].
	class instanceSide putCommentOnFile: stream.
	stream cr
]

{ #category : #'file in/out' }
ChangeSet >> fileOutOn: stream [
	"Write out all the changes the receiver knows about"

	| classList traits classes traitList list |
	self isEmpty ifTrue: [self inform: 'Warning: no changes to file out'].

	traits := self changedClasses select: [:each | each isTrait].
	classes := self changedClasses select: [:each | each isBehavior & each isTrait not].
	traitList := self class traitsOrder: traits asOrderedCollection.
	classList := self class classesOrder: classes asOrderedCollection.
	list := OrderedCollection new
		addAll: traitList;
		addAll: classList;
		yourself.

	"First put out rename, max classDef and comment changes."
	list do: [:aClass | self fileOutClassDefinition: aClass on: stream].

	"Then put out all the method changes"
	list do: [:aClass | self fileOutChangesFor: aClass on: stream].

	"Finally put out removals, final class defs and reorganization if any"
	list reverseDo: [:aClass | self fileOutPSFor: aClass on: stream].

	self classRemoves asSortedCollection do:
		[:aClassName | stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; cr]
]

{ #category : #'file in/out' }
ChangeSet >> fileOutPSFor: class on: stream [
	"Write out removals and initialization for this class."

	| dict classRecord currentDef |
	classRecord := changeRecords at: class name ifAbsent: [ ^ self ].
	dict := classRecord methodChangeTypes.
	dict keysSortedSafely
		do: [ :key |
			| changeType |
			changeType := dict at: key.
			(#(#remove #addedThenRemoved) includes: changeType)
				ifTrue: [
					stream
						nextChunkPut: class name , ' removeSelector: ' , key storeString;
						cr ]
				ifFalse: [
					(key = #initialize and: [ class isMeta ])
						ifTrue: [
							stream
								nextChunkPut: class soleInstance name , ' initialize';
								cr ] ] ].
	((classRecord includesChangeType: #change) and: [ (currentDef := class definitionString) ~= class definitionString ])
		ifTrue: [
			stream
				nextChunkPut: currentDef;
				cr ].
	(classRecord includesChangeType: #reorganize)
		ifFalse: [ ^ self ].
	class fileOutOrganizationOn: stream.
	stream cr
]

{ #category : #'file in/out' }
ChangeSet >> fileOutPostscriptOn: stream [
	"If the receiver has a postscript, put it out onto the stream.  "

	| aString |
	aString := self postscriptString.
	(aString notNil and: [ aString notEmpty ])
		ifFalse: [ ^ self ].
	stream nextChunkPut: aString.	"surroundedBySingleQuotes"
	stream
		cr;
		cr
]

{ #category : #'file in/out' }
ChangeSet >> fileOutPreambleOn: stream [
	"If the receiver has a preamble, put it out onto the stream.  "

	| aString |
	aString := self preambleString.
	(aString notNil and: [ aString notEmpty ])
		ifFalse: [ ^ self ].
	stream nextChunkPut: aString.	"surroundedBySingleQuotes"
	stream
		cr;
		cr
]

{ #category : #'moving changes' }
ChangeSet >> forgetAllChangesFoundIn: otherChangeSet [
	"Remove from the receiver all method changes found in aChangeSet. The intention is facilitate the process of factoring a large set of changes into disjoint change sets.  To use:  in a change sorter, copy over all the changes you want into some new change set, then use the subtract-other-side feature to subtract those changes from the larger change set, and continue in this manner."

	otherChangeSet == self ifTrue: [^ self].
	otherChangeSet changedClassNames do:
		[:className | self forgetChangesForClass: className in: otherChangeSet].
	self expungeEmptyClassChangeEntries
]

{ #category : #'moving changes' }
ChangeSet >> forgetChangesForClass: className in: otherChangeSet [
	"See forgetAllChangesFoundIn:.  Used in culling changeSets."

	(self changeRecorderFor: className) forgetChangesIn:
		(otherChangeSet changeRecorderFor: className)
]

{ #category : #'method changes' }
ChangeSet >> hasAnyChangeForSelector: aSelector [
	"Answer whether the receiver has any change under the given selector, whether it be add, change, or remove, for any class"

	^ changeRecords anySatisfy: [ :aRecord | aRecord changedSelectors includes: aSelector ]
]

{ #category : #accessing }
ChangeSet >> hasPostscript [

	^ postscript notNil
]

{ #category : #'moving changes' }
ChangeSet >> hasPreamble [

	^ preamble notNil
]

{ #category : #'file in/out' }
ChangeSet >> hasReportableSlip: aMethod [
	"Answer whether the receiver contains anything that should be brought
	to the attention of the author when filing out. Customize the lists here
	to suit your preferences."

	aMethod containsHalt ifTrue: [ ^ true ].
	(aMethod sendsSelector: #flag:) ifTrue: [ ^ true ].
	^ aMethod readsRef: (Smalltalk globals associationAt: #Transcript)
]

{ #category : #initialization }
ChangeSet >> initialize [

	"Initialize the receiver to be empty."

	super initialize.
	name ifNil: [ ^ self error: #'All changeSets must be registered, as in ChangeSorter newChangeSet' ].
	self clear
]

{ #category : #testing }
ChangeSet >> isEmpty [
	"Answer whether the receiver contains any elements."

	^ changeRecords isEmptyOrNil
]

{ #category : #testing }
ChangeSet >> isMoribund [
	"Answer whether the receiver is obsolete and about to die; part of an effort to get such guys cleared out from the change sorter."

	^ name isNil
]

{ #category : #testing }
ChangeSet >> isNumbered [
	"Answer whether a change set is numbered"

	^  self name startsWithDigit
]

{ #category : #'change logging' }
ChangeSet >> methodAdded: anEvent [

	self
		noteNewMethod: anEvent method
		forClass: anEvent methodClass
		selector: anEvent selector
		priorMethod: nil
]

{ #category : #accessing }
ChangeSet >> methodChanges [

	| methodChangeDict |
	methodChangeDict := Dictionary new.
	changeRecords associationsDo:
		[:assn | | changeTypes |
		changeTypes := assn value methodChangeTypes.
		changeTypes isEmpty ifFalse: [methodChangeDict at: assn key put: changeTypes]].
	^ methodChangeDict
]

{ #category : #'method changes' }
ChangeSet >> methodChangesAtClass: className [
	"Return an old-style dictionary of method change types."

	^ (changeRecords at: className ifAbsent: [ ^ Dictionary new ])
		  methodChangeTypes
]

{ #category : #'change logging' }
ChangeSet >> methodModified: anEvent [

	self
		noteNewMethod: anEvent newMethod
		forClass: anEvent methodClass
		selector: anEvent selector
		priorMethod: anEvent oldMethod
]

{ #category : #'change logging' }
ChangeSet >> methodRecategorized: anEvent [

	self reorganizeClass: anEvent methodClass
]

{ #category : #'change logging' }
ChangeSet >> methodRemoved: anEvent [

	self
		removeSelector: anEvent selector
		class: anEvent methodClass
		priorMethod: anEvent method
		lastSourcePointer: anEvent method sourcePointer
]

{ #category : #accessing }
ChangeSet >> name [
	"The name of this changeSet. If name is nil, we've got garbage.  Help to identify."

	^ name
		ifNil: [ '<no name -- garbage?>' ]
		ifNotNil: [ name ]
]

{ #category : #accessing }
ChangeSet >> name: anObject [

	name := anObject
]

{ #category : #'change logging' }
ChangeSet >> noteNewMethod: newMethod forClass: class selector: selector priorMethod: methodOrNil [

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteNewMethod: newMethod selector: selector priorMethod: methodOrNil
]

{ #category : #'class changes' }
ChangeSet >> noteRemovalOf: class [
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [ ^ self ].
	(self changeRecorderFor: class)
		noteChangeType: #remove
		fromClass: class.
	changeRecords removeKey: class class name ifAbsent: [  ]
]

{ #category : #testing }
ChangeSet >> okayToRemove [

	^ self okayToRemoveInforming: true
]

{ #category : #testing }
ChangeSet >> okayToRemoveInforming: aBoolean [
	"Answer whether it is okay to remove the receiver.  If aBoolean is true, inform the receiver if it is not okay"

	| aName |
	aName := self name.
	self == self class current ifTrue: [
		aBoolean ifTrue: [
			self inform: 'Cannot remove "' , aName , '"
because it is the
current change set.' ].
		^ false ].
	^ true
]

{ #category : #private }
ChangeSet >> oldNameFor: class [

	^ (changeRecords at: class name) priorName
]

{ #category : #'file in/out' }
ChangeSet >> postscript [
	"Answer the string representing the postscript.  "

	^ postscript ifNotNil: [
		  postscript isString
			  ifTrue: [ postscript ]
			  ifFalse: [ postscript contents asString ] ]
]

{ #category : #'file in/out' }
ChangeSet >> postscript: aString [
	"Answer the string representing the postscript."

	postscript := aString
]

{ #category : #accessing }
ChangeSet >> postscriptHasDependents [

	^ false
]

{ #category : #'file in/out' }
ChangeSet >> postscriptString [
	"Answer the string representing the postscript."

	^ self postscript
]

{ #category : #'file in/out' }
ChangeSet >> postscriptString: aString [
	"Establish aString as the new contents of the postscript."

	self postscript: aString
]

{ #category : #'file in/out' }
ChangeSet >> preamble [
	"Answer the string representing the preamble"

	^ preamble ifNotNil: [
		  preamble isString
			  ifTrue: [ preamble ]
			  ifFalse: [ preamble contents asString ] ]
]

{ #category : #'file in/out' }
ChangeSet >> preamble: aString [
	"Establish aString as the new contents of the preamble.  "

	preamble := aString
]

{ #category : #'file in/out' }
ChangeSet >> preambleString [
	"Answer the string representing the preamble"

	^self preamble
]

{ #category : #'file in/out' }
ChangeSet >> preambleString: aString [
	"Establish aString as the new contents of the preamble."

	self preamble: aString
]

{ #category : #'file in/out' }
ChangeSet >> preambleTemplate [
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	<script: 'ChangeSet current preambleTemplate inspect'>
	^ String streamContents: [ :strm |
		  strm nextPutAll: '"Change Set:'. "NOTE: fileIn recognizes preambles by this string."
		  strm
			  tab;
			  tab;
			  nextPutAll: self name.
		  strm
			  cr;
			  nextPutAll: 'Date:';
			  tab;
			  tab;
			  tab;
			  nextPutAll: Date today printString.
		  strm
			  cr;
			  cr;
			  nextPutAll: '<your descriptive text goes here>"' ]
]

{ #category : #printing }
ChangeSet >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' named ';
		nextPutAll: self name
]

{ #category : #'moving changes' }
ChangeSet >> removeClassAndMetaClassChanges: class [
	"Remove all memory of changes associated with this class and its metaclass."

	changeRecords
		removeKey: class name ifAbsent: [];
		removeKey: class class name ifAbsent: []
]

{ #category : #'moving changes' }
ChangeSet >> removeClassChanges: class [
	"Remove all memory of changes associated with this class"

	| cname |
	cname := class isString
		ifTrue: [ class ]
		ifFalse: [ class name ].
	changeRecords removeKey: cname ifAbsent: [  ]
]

{ #category : #accessing }
ChangeSet >> removePostscript [

	postscript := nil
]

{ #category : #'moving changes' }
ChangeSet >> removePreamble [

	preamble := nil
]

{ #category : #'change logging' }
ChangeSet >> removeSelector: selector class: class priorMethod: priorMethod lastSourcePointer: lastSourcePointer [
	"Include indication that a method has been forgotten."

	class wantsChangeSetLogging ifFalse: [ ^ self ].
	(self changeRecorderFor: class) noteRemoveSelector: selector priorMethod: priorMethod lastSourcePointer: lastSourcePointer
]

{ #category : #'method changes' }
ChangeSet >> removeSelectorChanges: selector class: class [
	"Remove all memory of changes associated with the argument, selector, in
	this class."

	| chgRecord |
	(chgRecord := changeRecords at: class name ifAbsent: [^ self])
		removeSelector: selector.
	chgRecord hasNoChanges ifTrue: [changeRecords removeKey: class name]
]

{ #category : #'change logging' }
ChangeSet >> renameClass: class from: oldName to: newName [
	"Include indication that a class has been renamed."

	| recorder oldMetaClassName newMetaClassName |
	(recorder := self changeRecorderFor: oldName)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.

	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: oldName.

	newMetaClassName := newName, ' class'.
	oldMetaClassName := oldName, ' class'.
	recorder := changeRecords at: oldMetaClassName ifAbsent: [^ nil].
	changeRecords at: newMetaClassName put: recorder.
	changeRecords removeKey: oldMetaClassName.
	recorder noteNewName: newMetaClassName
]

{ #category : #'class changes' }
ChangeSet >> reorganizeClass: class [
	"Include indication that a class was reorganized."

	self atClass: class add: #reorganize
]

{ #category : #'method changes' }
ChangeSet >> selectorsInClass: aClassName [
	"Used by a ChangeSorter to access the list methods."

	^ (changeRecords at: aClassName ifAbsent: [ ^ #() ])
		  changedSelectors
]

{ #category : #'class changes' }
ChangeSet >> trimHistory [
	"Drop non-essential history:  methods added and then removed, as well as rename and reorganization of newly-added classes."

	changeRecords do: [:chgRecord | chgRecord trimHistory]
]

{ #category : #initialization }
ChangeSet >> veryDeepCopyWith: deepCopier [
	"Return self; this is NOT the way to launch new change sets! Having this method here allows Change Sorters to be in parts bins"
]

{ #category : #initialization }
ChangeSet >> wither [
	"The receiver is to be clobbered. Clear it out."

	self clear.
	name := nil
]
