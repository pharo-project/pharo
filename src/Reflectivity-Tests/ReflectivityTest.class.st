Class {
	#name : #ReflectivityTest,
	#superclass : #TestCase,
	#instVars : [
		'count',
		'tag',
		'link',
		'link2'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #tagging }
ReflectivityTest >> increase [
	count ifNil: [ count := 0 ].
	count := count + 1.
]

{ #category : #tagging }
ReflectivityTest >> tagExec [
	tag := #yes
]

{ #category : #tagging }
ReflectivityTest >> tagExec: aTag [
	tag := aTag
]

{ #category : #running }
ReflectivityTest >> tearDown [
	link ifNotNil: [link uninstall].
	link2 ifNotNil: [link2 uninstall].
	tag := nil.
	count := 0.
	(ReflectivityExamples>>#exampleWithArg:)  recompile.
	super tearDown 
]

{ #category : #tests }
ReflectivityTest >> testTwoLinksUnstallOne [
	| node instance |
	node := (ReflectivityExamples>>#exampleWithArg:) ast.
	link := MetaLink new 
		metaObject: self; 
		selector: #increase.
		
	link2 := MetaLink new 
		metaObject: self; 
		selector: #increase.
		
	node link: link.
	node link: link2.
	self assert: (ReflectivityExamples>>#exampleWithArg:) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: ((instance exampleWithArg: 3) = 5).
	self assert: (count = 2 ).
	
	node removeLink: link2.
	self assert: ((instance exampleWithArg: 3) = 5).
	self assert: (count = 3).
]
