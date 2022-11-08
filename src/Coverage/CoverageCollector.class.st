"
I represent a code coverage session.

My instances can track the execution of methods and nodes and collects which are covered by execution and which are not.

Currently, compiled methods and sequences of statements (bodies of methods and block) can be tracked.
This is a compromise between efficency and precision that is good enough precition to track covered paths.

Here is a quick usage:

```
| collector coverage |
collector := CoverageCollector new. ""Instantiate""
collector methods: Point methods, Rectangle methods.  ""Configure with the methods to watch.""
coverage := collector runOn: [ (1@1 corner: 2@2) center ].  ""Setup, execute and teardown.""
(coverage methods includes: Point>>#x) >>> true.  ""Inspect the results""
(coverage nodes size > 10) >>> true. ""Covered paths are also available""
```

The `CoverageCollector>>#runOn:` methods is a high level method.
It calls the following fine grained methods in order:

* `CoverageCollector>>#install` to instrument the preselected methods
* `CoverageCollector>>#installOn:` to instrument the preselected methods ensuring `uninstall` is performed.
* `CoverageCollector>>#basicRun:` to watch the execution of a block.
* `CoverageCollector>>#uninstall` to remove the instrumentation on methods.
* `CoverageCollector>>#collectResult` to build and return a `CoverageResult` instance.
* `CoverageCollector>>#reset` to clear the annotation on executed methods.

Note: if `uninstall` is not called, some instrumentation may leaks.
You can use the global `CoverageCollector class>>#uninstallAll` to remove all instrumentation if needed.

Warning: because of the instrumentation, new instances of `CompiledMethod` can be created to replace existing one during the coverage computation. This means that the identity of method objects returned by expression like `Point>>#x` could be not reliable and that `=` and related methods may not behave as you expect if you use lower level methods.
"
Class {
	#name : #CoverageCollector,
	#superclass : #Object,
	#instVars : [
		'metalink',
		'methods',
		'nodes'
	],
	#category : #'Coverage-Base'
}

{ #category : #installation }
CoverageCollector class >> uninstalAll [

	"If for some reason metalinks are not removed, force a full image cleaning"

	<script>
	MetaLink allInstancesDo: [ :each | 
		(each selector == #tagExecuted and: [ each metaObject == #node ]) 
			ifTrue: [ each uninstall ] ]
]

{ #category : #basic }
CoverageCollector >> basicCollectResult [

	"Collect the results but does not reset the annotations."

	| res |
	res := CoverageResult new.
	res collector: self.
	res methods: (methods select: [ :m | m ast hasBeenExecuted ]).
	res nodes: IdentitySet new.
	nodes do: [ :node | 
			node hasBeenExecuted ifTrue: [ res nodes add: node ] ].

	^ res
]

{ #category : #basic }
CoverageCollector >> basicRun: aBlock [

	"Just run the block. We consider that the correct setup is done and that a teardown will be performed."

	^ aBlock value
]

{ #category : #basic }
CoverageCollector >> collectResult [

	"Collect the results.
	This also remaps the methods and resets the annotations."

	| res |
	self remapMethods.
	res := self basicCollectResult.
	self reset.
	^ res
]

{ #category : #initialization }
CoverageCollector >> initialize [

	metalink := MetaLink new
		            selector: #tagExecuted;
		            metaObject: #node.
	methods := IdentitySet new.
	"Need IdentitySet here because we want to distinguish equivalent nodes."
	nodes := IdentitySet new.
]

{ #category : #basic }
CoverageCollector >> install [

	"Prepare the metalink. This can be slow.
	
	Sequence nodes are also prepared. This enable a basic path coverage."

	methods do: [ :meth | 
		meth ast nodesDo: [ :node | 
			node isSequence ifTrue: [ 
				node link: metalink.
				nodes add: node ] ].
		meth ast link: metalink.
	 ]
]

{ #category : #running }
CoverageCollector >> installOn: aBlock [

	"just run a block, with installation and uninstallation."

	self install.
	aBlock ensure: [ self uninstall ].
]

{ #category : #accessing }
CoverageCollector >> metalink [

	^ metalink
]

{ #category : #accessing }
CoverageCollector >> methods [

	^ methods
]

{ #category : #accessing }
CoverageCollector >> methods: anObject [

	methods := anObject
]

{ #category : #accessing }
CoverageCollector >> nodes [

	^ nodes
]

{ #category : #printing }
CoverageCollector >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' methods: ';
		print: methods size;
		nextPutAll: ' nodes: ';
		print: nodes size
]

{ #category : #private }
CoverageCollector >> remapMethods [

	"Because of metalinks, CompiledMethods can by recompiled, so this method is called to updates the list of methods with the 'current' list of methods."
	^ methods := methods collect: [ :m | m classBinding value >> m selector ]
]

{ #category : #initialization }
CoverageCollector >> reset [

	"Cleanup the annotations, metalinks remain installed.
	You should call if before you measure something."

	methods do: [ :meth | meth ast removeProperty: #tagExecuted ifAbsent: [ ] ].
	nodes do: [ :each | each removeProperty: #tagExecuted ifAbsent: [ ] ].
]

{ #category : #running }
CoverageCollector >> runOn: aBlock [

	"Fully run self, with installation, execution and uninstallation.
	
	Returns a CoverageResult."

	self installOn: [
		self reset.
		self basicRun: aBlock ].
	^ self collectResult
]

{ #category : #basic }
CoverageCollector >> uninstall [

	"Remove the instrumentation.
	
	Use `runOn:` or `installOn:` that automatically ensures that `uninstall` is called."

	metalink uninstall
]
