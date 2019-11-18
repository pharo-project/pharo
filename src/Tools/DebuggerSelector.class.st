"
This class is not actually a debugger, but when asked to open on a debug session, it will look at all the debuggers available in the system and open the best one for the particular DebugSession being opened.

For the conditions a class must satisfy in order to be considered a debugger class, look at the #isThisClassADebugger method.

If the variable mainDebuggerClass is set to hold a debugger class, it will be used in priority to open debug sessions (unless it cannot open them, in which case we fallback to the default algorithm)

# Variables
- mainDebuggerClass  | a Class
- subscribersOnMainDebuggerClassUpdate  |  WeakOrderedCollection<Object>
"
Class {
	#name : #DebuggerSelector,
	#superclass : #Object,
	#classVars : [
		'mainDebuggerClass',
		'subscribersOnMainDebuggerClassUpdate'
	],
	#category : #'Tools-Debugger'
}

{ #category : #'publish events' }
DebuggerSelector class >> addSubscriberOnMainDebuggerClassUpdate: anObject [
	self subscribersOnMainDebuggerClassUpdate add: anObject
]

{ #category : #initialization }
DebuggerSelector class >> getDebuggerClasses [
	^ (((PragmaCollector filter: [:prg | prg selector = #'iAmADebugger' ]) reset collected) collect: [ :debuggerMethod | debuggerMethod methodClass soleInstance ]) select: [:aClass | self isThisClassADebugger: aClass] yourself.
]

{ #category : #initialization }
DebuggerSelector class >> isThisClassADebugger: aClass [
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
DebuggerSelector class >> mainDebuggerClass [
	^ mainDebuggerClass
]

{ #category : #accessor }
DebuggerSelector class >> mainDebuggerClass: aClass [
	(aClass isNil or: [self isThisClassADebugger: aClass]) ifFalse: [ CannotSetAsMainDebuggerAClassThatIsNotADebuggerClass signal ].
	mainDebuggerClass := aClass.
	self notifySubscribersOfAMainDebuggerClassUpdate
]

{ #category : #'publish events' }
DebuggerSelector class >> notifySubscribersOfAMainDebuggerClassUpdate [
	self subscribersOnMainDebuggerClassUpdate do: [ :subscriber_ | subscriber_ ifNotNil: [:subscriber | subscriber onMainDebuggerClassUpdate ]]
]

{ #category : #'publish events' }
DebuggerSelector class >> removeSubscriberOnMainDebuggerClassUpdate: anObject [
	self subscribersOnMainDebuggerClassUpdate remove: anObject ifAbsent: [ "It's a weak collection, its elements can be nilled by the garbage collector at any time if they are not referenced elsewhere, so it can happen not to find the object to remove because it has already been nilled" ]
]

{ #category : #'open debugger' }
DebuggerSelector class >> selectDebuggerAndOpen: aDebugSession withFullView: aBool andNotification: aString [
	"Open @aDebugSession with the most suitable debugger class in the system. Look at the comments below for the detailed algorithm selecting the most suitable debugger"
	| debuggerClasses |
	"If a main debugger has been set (variable mainDebuggerClass), we try it first"
	[(self mainDebuggerClass ifNotNil: [:theMainDebuggerClass | self tryToOpenDebugSession: aDebugSession withDebuggerClass: theMainDebuggerClass withFullView: aBool andNotification: aString ]) ifTrue: [ ^ self "the main debugger class managed to open @aDebugSession. We are done here" ]] on: Exception do: ["Prevent exceptions potentially signalled by the mainDebuggerClass from propagating, to avoid infinite debugger loops"].
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
DebuggerSelector class >> setNoMainDebuggerClass [
	"Removes the main debugger class if there was any"
	self mainDebuggerClass: nil
]

{ #category : #'publish events' }
DebuggerSelector class >> subscribersOnMainDebuggerClassUpdate [
	subscribersOnMainDebuggerClassUpdate ifNil: [ subscribersOnMainDebuggerClassUpdate := WeakOrderedCollection new ].
	^ subscribersOnMainDebuggerClassUpdate
]

{ #category : #helper }
DebuggerSelector class >> tryToOpenDebugSession: aDebugSession withDebuggerClass: aDebuggerClass withFullView: aBool andNotification: aString [
	"Tries to open @aDebugSession with debugger @aDebuggerClass. If an Error is raised doing so, returns false, otherwise return true"
	[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. ^ true] on: Error do: [ ^ false ]
]
