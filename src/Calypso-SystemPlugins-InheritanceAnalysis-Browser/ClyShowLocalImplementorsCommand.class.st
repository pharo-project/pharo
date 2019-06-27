"
I am a root of commands hierarchy which are supposed to show implementors of selected methods in local class scope of these methods.
My subclasses should define what exact local scope they allow to browse. They should implement following method:

- createInheritanceScopeFrom: classes 

It should return parcicular class scope instance.
Also my subclasses can define default selection in spawned query browser. They should override method:

- selectMethodsIn: aQueryBrowser

My instance should be created on method selection and browser.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : #ClyShowLocalImplementorsCommand,
	#superclass : #SycMethodCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #testing }
ClyShowLocalImplementorsCommand class >> isAbstract [
	^self = ClyShowLocalImplementorsCommand 
]

{ #category : #execution }
ClyShowLocalImplementorsCommand >> createInheritanceScopeFrom: classes [

	self subclassResponsibility 
]

{ #category : #execution }
ClyShowLocalImplementorsCommand >> createQueryScope [

	| classes scope |
	classes := methods collect: [ :each | each origin].
	
	scope := self createInheritanceScopeFrom: classes.
	scope := scope withMetaLevel: ClyClassScope.
	^scope
]

{ #category : #execution }
ClyShowLocalImplementorsCommand >> execute [

	| selectors query |
	selectors := methods collect: [ :each | each selector].
	
	query := 	ClyMessageImplementorsQuery ofAny: selectors from: self createQueryScope.
	browser spawnQueryBrowserOn: query withState: [:queryBrowser |
		self selectMethodsIn: queryBrowser ]
]

{ #category : #execution }
ClyShowLocalImplementorsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]

{ #category : #execution }
ClyShowLocalImplementorsCommand >> selectMethodsIn: aQueryBrowser [ 
]
