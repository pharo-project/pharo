"
A ChangeSorterModel is a model used by Change Sorter UIs for computation
"
Class {
	#name : #ChangeSorterModel,
	#superclass : #AbstractTool,
	#classVars : [
		'ClassDescriptionsMap'
	],
	#category : #'NewTools-ChangeSorter'
}

{ #category : #accessing }
ChangeSorterModel class >> classDescriptionsMap [
	^ ClassDescriptionsMap
]

{ #category : #accessing }
ChangeSorterModel class >> classDescriptionsMap: anObject [
	ClassDescriptionsMap := anObject
]

{ #category : #defaults }
ChangeSorterModel class >> defaultClassDescriptionsMap [
	^ Dictionary new
		at: #remove put: 'Entire class was removed.';
		at: #addedThenRemoved put: 'Class was added then removed.';
		at: #rename put: 'Class name was changed.';
		at: #add put: 'Class definition was added.';
		at: #change put: 'Class definition was changed.';
		at: #reorganize put: 'Class organization was changed.';
		at: #comment put: 'New class comment.';
		yourself
]

{ #category : #'class initialization' }
ChangeSorterModel class >> initialize [
	self classDescriptionsMap: self defaultClassDescriptionsMap
]

{ #category : #'change set' }
ChangeSorterModel >> addPreambleTo: aChangeSet [

	aChangeSet assurePreambleExists
]

{ #category : #environment }
ChangeSorterModel >> allChanges [

	^ ChangeSet allChangeSets reverse
]

{ #category : #text }
ChangeSorterModel >> buildChangeSetDescriptionFor: changeSet [
		
	^ changeSet
		ifNil: [ '' ] 
		ifNotNil: [ changeSet preambleString ifNil: ['']]
]

{ #category : #text }
ChangeSorterModel >> buildClassDescriptionFor: changeSet class: class [
	^ String streamContents: [ :stream |
			(changeSet classChangeAt: class name)
				do: [ :each | stream nextPutAll: (self classDescriptionsMap at: each) ]
				separatedBy: [ stream cr ] ]
]

{ #category : #text }
ChangeSorterModel >> buildSelectorDescriptionFor: changeSet class: class selector: selector [

	| changeType code |
	changeType := changeSet atSelector: selector class: class.
	
	changeType == #remove
		ifTrue: [^ 'Method has been removed (see versions)'].
	changeType == #addedThenRemoved
		ifTrue: [^ 'Added then removed (see versions)']. 
	class
		ifNil: [^ 'Method was added, but cannot be found!'].
	(class includesSelector: selector)
		ifFalse: [^ 'Method was added, but cannot be found!'].
	code := class sourceCodeAt: selector.
		
	^ code asText
]

{ #category : #accessing }
ChangeSorterModel >> classDescriptionsMap [

	^ self class classDescriptionsMap
]

{ #category : #'change set' }
ChangeSorterModel >> copyAllChangesFrom: source to: destination [

	destination assimilateAllChangesFoundIn: source
]

{ #category : #class }
ChangeSorterModel >> copyClass: aClass from: source to: destination [

	destination absorbClass: aClass name from: source.
]

{ #category : #message }
ChangeSorterModel >> copySelector: selector inClass: class from: source to: destination [

	destination
		absorbMethod: selector 
		class: class 
		from:source
]

{ #category : #'change set' }
ChangeSorterModel >> createNewSet [

	^ ChangeSet newChangeSet
]

{ #category : #'change set' }
ChangeSorterModel >> currentChangeSet [

	^ ChangeSet current 
]

{ #category : #class }
ChangeSorterModel >> fileOutClass: aClass from: aChangeSet [
	"this is a hack!! makes a new change set, called the class name, adds author initials to try to make a unique change set name, files it out and removes it. kfr 16 june 2000"
	"Method copied from ChangeSorter"
	
	| aSet |
      aSet := ChangeSet newChangeSet: aClass name.
	aSet ifNil:[ ^self ].
	aSet absorbClass: aClass name from: aChangeSet.
	aSet fileOut.
	ChangeSet removeChangeSet: aSet.
]

{ #category : #message }
ChangeSorterModel >> fileOutSelector: selector from: aClass [
	
	aClass fileOutMethod: selector
]

{ #category : #'change set' }
ChangeSorterModel >> findChangeSetIn: aCollectionOfChangeSets [
	"Search for a changeSet by name.  Pop up a menu of all changeSets whose name contains the string entered by the user.  If only one matches, then the pop-up menu is bypassed"
	| index pattern candidates nameList |

	self okToChange ifFalse: [^ self].
	pattern := UIManager default request: 'ChangeSet name or fragment?'.
	pattern isEmptyOrNil ifTrue: [^ self].
	nameList := aCollectionOfChangeSets collect: [ : each | each name ].
	candidates := aCollectionOfChangeSets select:
			[:c | (nameList includes: c name) and: 
				[c name includesSubstring: pattern caseSensitive: false]].
	candidates size = 0 ifTrue: [ ^ self inform: 'No matching change sets.' ].
	candidates size = 1 ifTrue: [ ^ candidates first ].
	index := UIManager default chooseFrom: (candidates collect: [:each | each name]).
	index = 0 ifFalse: [ ^ (candidates at: index) ].
]

{ #category : #message }
ChangeSorterModel >> forgetSelector: selector inClass: aClass fromChangeSet: aChangeSet [

	aChangeSet
			removeSelectorChanges: selector 
			class: aClass
]

{ #category : #'change set' }
ChangeSorterModel >> removeChangeSet: aChangeSet prompting: doPrompt [
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName changeSetNumber msg |
	aName := aChangeSet name.
	aChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(aChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message := 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg := aChangeSet hasPreamble
			ifTrue:
				[aChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[aChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	"Go ahead and remove the change set"
	changeSetNumber := aChangeSet name initialIntegerOrNil.
	changeSetNumber ifNotNil: [SystemVersion current unregisterUpdate: changeSetNumber].
	ChangeSet removeChangeSet: aChangeSet.
]

{ #category : #class }
ChangeSorterModel >> removeClass: class from: changeSet [

	changeSet removeClassChanges: class.
]

{ #category : #'change set' }
ChangeSorterModel >> removePostscriptFrom: aChange [

	(aChange hasPostscript and: [ aChange postscriptHasDependents ]) ifTrue:
		[^ self inform:
'Cannot remove the postscript right
now because there is at least one
window open on that postscript.
Close that window and try again.'].

	aChange removePostscript
]

{ #category : #'change set' }
ChangeSorterModel >> rename: aChangeSet [
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	| newName |
	newName := UIManager default request: 'New name for this change set'
						initialAnswer: aChangeSet name.
						
	(newName = aChangeSet name or: [newName isEmptyOrNil]) ifTrue:
			[ ^ UIManager default inform: 'Please give a new name' ].

	(ChangeSet named: newName) ifNotNil:
			[ ^ UIManager default inform: 'Sorry that name is already used' ].

	aChangeSet name: newName.
	^ aChangeSet
]

{ #category : #text }
ChangeSorterModel >> setContentsOfChangeSet: changeSet forClass: class andSelector: selector [
	"return the source code that shows in the bottom pane"

	class
		ifNil: [
			"Only the change set is currently selected"
			^ self buildChangeSetDescriptionFor: changeSet ].
	selector 
		ifNil: [ 
			"class is selected but not the selector"
			^ self buildClassDescriptionFor: changeSet class: class ]
		ifNotNil: [
			"a class and a selector are selected"
			^ self buildSelectorDescriptionFor: changeSet class: class selector: selector ]
]

{ #category : #'change set' }
ChangeSorterModel >> setCurrentChangeSet: aChangeSet [

	ChangeSet newChanges: aChangeSet
	
]

{ #category : #'change set' }
ChangeSorterModel >> submerge: source into: destination [
	"Copy the contents of the receiver to the other side, then remove the receiver -- all after checking that all is well."
	|   message |
	source == destination
		ifTrue: [ ^ false ].
	source isEmpty
		ifTrue: [ 
			self inform: 'Nothing to copy.  To remove,
simply choose "remove".'.
			^ false ].

	source okayToRemove ifFalse: [^ false].
	message := 'Please confirm:  copy all changes
in "', source name, '" into "', destination name, '"
and then destroy the change set
named "', source name, '"?'.
 
	(self confirm: message) ifFalse: [^ false].

	(source hasPreamble or: [source hasPostscript]) ifTrue:
		[(self confirm: 
'Caution!  This change set has a preamble or
a postscript or both.  If you submerge it into
the other side, these will be lost.
Do you really want to go ahead with this?') ifFalse: [^ false]].

	self copyAllChangesFrom: source to: destination.
	self removeChangeSet: source prompting: false.
	^ true
]

{ #category : #'change set' }
ChangeSorterModel >> subtractFrom: source to: destination [

	source forgetAllChangesFoundIn: destination
]
