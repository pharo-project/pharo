"
The purpose of this class is to select ""the best"" debugger to open a debug session (a.k.a. execution that signalled an uncaught exception).
-> See method #selectDebuggerAndOpen:withFullView:withNotification:

This class finds all the debugger classes present in the image with #getDebuggerClasses and the method testing whether a class is a debugger class or not is #isThisClassADebugger.

This class implements the singleton design pattern. Its singleton can be obtained by executing `DebuggerSelector singleton`.

# Variables
- mainDebuggerClass  | a Class
- subscribersOnMainDebuggerClassUpdate  |  WeakOrderedCollection<Object>

# Long Description
**The Point**
The idea of `DebuggerSelector` is to have a generic system for choosing ""the most suitable"" debugger to open a given DebugSession. A system like this is already in place in some fashion, but it is internal to the GTDebugger (you can see it at play when you debug a test: the debugger looks a bit different and shows test-specific information, because it's actually the GTTestDebugger that was opened), so it is not easy to add new non-GT debuggers.

**Short Description**
`DebuggerSelector` finds all the debugger classes of the system, ask them how they rank the DebugSession to be opened, and opens the DebugSession with the highest-ranking debugger. If this opening fails (because the debugger signals an Error), then `DebuggerSelector` tries to open the DebugSession with the second-highest ranking debugger and so on...

**What is a Debugger class?**
A debugger class is a class that implements all the methods from the AbstractDebugger class that are marked as SubclassResponsibility. For now, that is:
- `iAmADebugger`: This method simply needs to contain the `<iAmADebugger>` pragma, and is just there as a marker.
- `rankDebugSession: aDebugSession`: This method returns the rank of this debugger on the provided DebugSession. If the debugger absolutely cannot open the DebugSession, it should signal the `DebugSessionCannotBeOpenedByThisDebugger` exception. For example, a debugger that is specialised to debug test executions will return a high number if the DebugSession is that of a test execution, and signal the exception if it is not.
- `openOn: aDebugSession withFullView: aBool andNotification: aString`: This method should open the provided DebugSession in that debugger. It is a good place to call `#keepAlive:` on the DebugSession to ensure it does not get terminated by `DebugSessionStorage` while the debugger is open on it.
An important point is that debugger classes do not need to inherit from the `AbstractDebugger` class to be considered as debugger classes. They only need to implement the three (class-side) methods detailed above.

**More Details on the Debugger Selection Algorithm**
The algorithm outlined above gives the main idea. In addition to that:
- Debugger classes that signal the `DebugSessionCannotBeOpenedByThisDebugger` exception when asked to rank the DebugSession cannot be selected
- `DebuggerSelector` has a `mainDebuggerClass` variable. If a debugger class is in this variable, `DebuggerSelector` will always attempt to open the DebugSession with it first, and only if that signals an Error will it fall back on the default algorithm. A use for this is to manually set a specific debugger for images in production mode, for example one that only logs the exception in a file but does not open a graphical debugger allowing the user to modify code.

"
Class {
	#name : #DebuggerSelector,
	#superclass : #Object,
	#instVars : [
		'mainDebuggerClass',
		'subscribersOnMainDebuggerClassUpdate'
	],
	#classVars : [
		'singleton'
	],
	#category : #'Tools-Debugger'
}

{ #category : #'instance creation' }
DebuggerSelector class >> singleton [
	singleton ifNil: [ singleton := self new ].
	^ singleton
]

{ #category : #'publish events' }
DebuggerSelector >> addSubscriberOnMainDebuggerClassUpdate: anObject [
	self subscribersOnMainDebuggerClassUpdate add: anObject
]

{ #category : #initialization }
DebuggerSelector >> getDebuggerClasses [
	| result classesWithDummyDebuggerPragma |
	result := (((PragmaCollector filter: [:prg | prg selector = #'iAmADebugger' ]) reset collected) collect: [ :debuggerMethod | debuggerMethod methodClass soleInstance ]) select: [:aClass | self isThisClassADebugger: aClass] yourself.
	self ignoreDummyDebuggerForTests ifTrue: [ 
		"If we have to remove dummy debuggers from the list (typically during normal uses of this class), remove all debugger classes that have the #iAmADummyDebuggerForTest pragma from the list. Those dummy debuggers are there for testing purposes"
		classesWithDummyDebuggerPragma := ((PragmaCollector filter: [:prg | prg selector = #'iAmADummyDebuggerForTest' ]) reset collected) collect: [ :debuggerMethod | debuggerMethod methodClass soleInstance ].	
		result removeAll: classesWithDummyDebuggerPragma.
	].
	^ result
]

{ #category : #initialization }
DebuggerSelector >> ignoreDummyDebuggerForTests [
	"If true, this class will ignore the DummyDebuggerForTests class when selecting a debugger. This is intended to be overriden for testing purposes"
	^ true
]

{ #category : #initialization }
DebuggerSelector >> isThisClassADebugger: aClass [
	"Returns whether @aClass implements the required methods to be considered a debugger class. It must:
	- have a #iAmADebugger method, with the #iAmADebugger pragma
	- implement all the class methods from class AbstractDebugger that are marked as being subclass responsibility
	- implement all the instance-side methods from class AbstractDebugger that are marked as being subclass responsibility"
	| requiredClassMethods aClassClassMethods requiredMethods aClassMethods |
	(aClass class allMethods anySatisfy: [:meth | meth selector = #iAmADebugger & (meth hasPragmaNamed: #iAmADebugger)]) ifFalse: [ ^ false ]. "aClass must have a class method named #iAmADebugger and having the #iAmADebugger pragma"
	
	requiredClassMethods := (AbstractDebugger class allMethods select: [:aMethod | aMethod methodClass = AbstractDebugger class]) select: [:aMethod | aMethod isSubclassResponsibility]. "Lists all class methods exactly (i.e. not inherited) on class AbstractDebugger that are marked as being subclass responsibility"
	aClassClassMethods := aClass class allMethods. "List all class methods on class aClass (including inherited class methods)"
	aClassClassMethods := aClassClassMethods reject: [ :aMethod | aMethod isSubclassResponsibility ]. "Remove class methods that are marked as being subclass responsibility. In particular, this removes methods inherited from the AbstractDebugger class if aClass inherits from it"
	((aClassClassMethods collect: [:aMethod | aMethod selector]) includesAll: (requiredClassMethods collect: [:aMethod | aMethod selector])) ifFalse: [ ^ false ]. "Requires that aClass implements all the methods from the requiredClassMethods list"
	
	"Then we do the same thing, but for methods on the instance side"
	requiredMethods := (AbstractDebugger allMethods select: [:aMethod | aMethod methodClass = AbstractDebugger]) select: [:aMethod | aMethod isSubclassResponsibility].
	aClassMethods := aClass class allMethods.
	aClassMethods := aClassMethods reject: [ :aMethod | aMethod isSubclassResponsibility ].
	((aClassMethods collect: [:aMethod | aMethod selector]) includesAll: (requiredMethods collect: [:aMethod | aMethod selector])) ifFalse: [ ^ false ].
	
	^ true.
	
]

{ #category : #accessor }
DebuggerSelector >> mainDebuggerClass [
	^ mainDebuggerClass
]

{ #category : #accessor }
DebuggerSelector >> mainDebuggerClass: aClass [
	(aClass isNil or: [self isThisClassADebugger: aClass]) ifFalse: [ CannotSetAsMainDebuggerAClassThatIsNotADebuggerClass signal ].
	mainDebuggerClass := aClass.
	self notifySubscribersOfAMainDebuggerClassUpdate
]

{ #category : #'publish events' }
DebuggerSelector >> notifySubscribersOfAMainDebuggerClassUpdate [
	self subscribersOnMainDebuggerClassUpdate do: [ :subscriber_ | subscriber_ ifNotNil: [:subscriber | subscriber onMainDebuggerClassUpdate ]]
]

{ #category : #'publish events' }
DebuggerSelector >> removeSubscriberOnMainDebuggerClassUpdate: anObject [
	self subscribersOnMainDebuggerClassUpdate remove: anObject ifAbsent: [ "It's a weak collection, its elements can be nilled by the garbage collector at any time if they are not referenced elsewhere, so it can happen not to find the object to remove because it has already been nilled" ]
]

{ #category : #'open debugger' }
DebuggerSelector >> selectDebuggerAndOpen: aDebugSession withFullView: aBool andNotification: aString [
	"Open @aDebugSession with the most suitable debugger class in the system. Look at the comments below for the detailed algorithm selecting the most suitable debugger"
	"The @aBool and @aString arguments do not seem super useful, so you can probably get away with respectively giving true and nil as values for them"
	| debuggerClasses |
	"If a main debugger has been set (variable mainDebuggerClass), we try it first"
	[(self mainDebuggerClass ifNotNil: [:theMainDebuggerClass | self tryToOpenDebugSession: aDebugSession withDebuggerClass: theMainDebuggerClass withFullView: aBool andNotification: aString ]) ifTrue: [ ^ self "the main debugger class managed to open @aDebugSession. We are done here" ]] on: Error do: ["Prevent exceptions potentially signalled by the mainDebuggerClass from propagating, to avoid infinite debugger loops"].
	"If the main debugger class cannot open @aDebugSession, we use the standard algorithm to find a suitable debugger class"
	debuggerClasses := self getDebuggerClasses. "Get all debugger classes in the system"
	debuggerClasses := debuggerClasses select: [:aDebuggerClass | [aDebuggerClass rankDebugSession: aDebugSession. true] on: DebugSessionCannotBeOpenedByThisDebugger do: [false]]. "Exclude all debugger classes that signal the DebugSessionCannotBeOpenedByThisDebugger exception when asked to rank @aDebugSession, as these cannot open @aDebugSession"
	debuggerClasses := ((debuggerClasses collect: [ :aDebuggerClass | { aDebuggerClass. aDebuggerClass rankDebugSession: aDebugSession } ]) sort: [ :val1 :val2 | (val1 at: 2) >= (val2 at: 2) ]) collect: [:couple | couple at: 1]. "Ask each debugger class to rank @aDebugSession, and sort them from highest rank to lowest."
	debuggerClasses do: [ :aDebuggerClass |
		(self tryToOpenDebugSession: aDebugSession withDebuggerClass: aDebuggerClass withFullView: aBool andNotification: aString) ifTrue: [^ self "We opened @aDebugSession with a suitable debugger. We are done here"].
	]. "Try to open @aDebugSession with the highest ranking debugger. If this raises an Error, try with the second highest ranking debugger instead. If this raises an Error, try with the third highest..."
	self inform: 'No debugger could open this debug session'. 
]

{ #category : #initialization }
DebuggerSelector >> setNoMainDebuggerClass [
	"Removes the main debugger class if there was any"
	self mainDebuggerClass: nil
]

{ #category : #'publish events' }
DebuggerSelector >> subscribersOnMainDebuggerClassUpdate [
	subscribersOnMainDebuggerClassUpdate ifNil: [ subscribersOnMainDebuggerClassUpdate := WeakOrderedCollection new ].
	^ subscribersOnMainDebuggerClassUpdate
]

{ #category : #helper }
DebuggerSelector >> tryToOpenDebugSession: aDebugSession withDebuggerClass: aDebuggerClass withFullView: aBool andNotification: aString [
	"Tries to open @aDebugSession with debugger @aDebuggerClass. If an Error is raised doing so, returns false, otherwise return true"
	[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. ^ true] on: Error do: [ ^ false ]
]
