"
Used to measure test coverage for Hudson reports. It uses methods-as-objects approach.

You may use it do find methods that are not called in your package with code like this:

```language=Pharo&caption=Beautiful
| methods wrappers |
methods := 'My Package' asPackage methods.
wrappers := methods collect: [ :each | HDTestCoverage on: each ].
wrappers do: [ :each | each install ].

""open the application here""
	
wrappers do: [ :each | each uninstall ].
wrappers := wrappers reject: [:each | each hasRun].
(wrappers collect: [:each | each reference]) inspect.
```

"
Class {
	#name : #HDTestCoverage,
	#superclass : #ProtoObject,
	#instVars : [
		'hasRun',
		'reference',
		'method'
	],
	#category : #'JenkinsTools-ExtraReports'
}

{ #category : #'instance creation' }
HDTestCoverage class >> on: aMethodReference [
	^ self new initializeOn: aMethodReference
]

{ #category : #private }
HDTestCoverage >> doesNotUnderstand: aMessage [
	^ method perform: aMessage selector withArguments: aMessage arguments
]

{ #category : #testing }
HDTestCoverage >> hasRun [
	^ hasRun
]

{ #category : #initialization }
HDTestCoverage >> initializeOn: aMethodReference [
	hasRun := false.
	reference := aMethodReference.
	method := reference compiledMethod
]

{ #category : #actions }
HDTestCoverage >> install [
	reference methodClass methodDict
		at: reference selector
		put: self.
]

{ #category : #private }
HDTestCoverage >> mark [
	hasRun := true
]

{ #category : #accessing }
HDTestCoverage >> method [
	^ method
]

{ #category : #accessing }
HDTestCoverage >> reference [
	^ reference
]

{ #category : #evaluation }
HDTestCoverage >> run: aSelector with: anArray in: aReceiver [
	self mark; uninstall.
	^ aReceiver withArgs: anArray executeMethod: method
]

{ #category : #actions }
HDTestCoverage >> uninstall [
	reference methodClass methodDict
		at: reference selector
		put: method.
]
