Class {
	#name : 'ReflectivityTest',
	#superclass : 'ReflectivityTestCase',
	#instVars : [
		'count',
		'tag',
		'link',
		'link2',
		'cache'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'tagging' }
ReflectivityTest >> increase [
	count := count + 1
]

{ #category : 'running' }
ReflectivityTest >> setUp [
	super setUp.
	count := 0.
	cache := OCASTCache default
]

{ #category : 'tagging' }
ReflectivityTest >> tagExec [
	tag := #yes
]

{ #category : 'tagging' }
ReflectivityTest >> tagExec: aTag [
	tag := aTag
]

{ #category : 'running' }
ReflectivityTest >> tearDown [
	link ifNotNil: [link uninstall].
	link2 ifNotNil: [link2 uninstall].
	tag := nil.
	count := 0.
	OCASTCache default: cache.
	super tearDown
]

{ #category : 'tests' }
ReflectivityTest >> testDisableLink [
	| node instance |
	node := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #increase.
	node link: link.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 1.
	link disable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 1
]

{ #category : 'tests' }
ReflectivityTest >> testDisableThenCleanASTCacheThenEnableLink [
	| node instance |
	node := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #increase.
	node link: link.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 1.
	link disable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 1.
	"Clean the AST cache"
	OCASTCache default reset.
	link enable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 2
]

{ #category : 'tests' }
ReflectivityTest >> testDisableThenEnableLink [
	| node instance |
	node := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #increase.
	node link: link.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 1.
	link disable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 1.
	link enable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 2.
	self assert: link condition isNil
]

{ #category : 'tests' }
ReflectivityTest >> testDisableThenEnableLinkWithCondition [
	| node instance |
	node := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		condition: false;
		selector: #increase.
	node link: link.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 0.
	link disable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 0.
	link enable.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 0.
	self assert: link condition equals: false
]

{ #category : 'tests' }
ReflectivityTest >> testTwoLinksUninstallOne [
	| node instance |
	node := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #increase.
	link2 := MetaLink new
		metaObject: self;
		selector: #increase.
	node link: link.
	node link: link2.
	self
		assert: (ReflectivityExamples >> #exampleWithArg:) class
		equals: ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 2.
	node removeLink: link2.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: count equals: 3
]
