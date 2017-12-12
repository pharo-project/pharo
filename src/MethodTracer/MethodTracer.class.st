"
allBehaviors := ((Smalltalk globals values select: [ :each | each isClassOrTrait ] ) flatCollect: [ :each | { each. each classSide } ]).

MethodTracer installOn: (allMethods shuffled).
MethodTracer uninstall.
(MethodTracer wrappers select: [ :each | each hasRun ]) size.
"
Class {
	#name : #MethodTracer,
	#superclass : #ProtoObject,
	#instVars : [
		'hasRun',
		'methodClass',
		'selector',
		'method'
	],
	#classInstVars : [
		'wrappers'
	],
	#category : #MethodTracer
}

{ #category : #'instance creation' }
MethodTracer class >> canInstall: aMethod [

		| classBlackList blackList |
	
	classBlackList := {
		MethodTracer. MethodTracer class. Context. Context class.

	}.
	
	classBlackList do: [ :each | (aMethod methodClass == each) ifTrue: [ ^ false ] ].
	
	blackList := {
		ArrayedCollection. #size.
		SmallInteger. #'\\'.
		ProtoObject. #isNil.
		ProtoObject. #basicIdentityHash.
		Object. #basicAt:.
		Object. #at:put:.
		Behavior. #methodDict.
		HashedCollection. #findElementOrNil:.
		MethodDictionary. #at:put:.
		MethodDictionary. #scanFor:.
		MethodDictionary. #swap:with:.
		BlockClosure. #value:.
		HashedCollection. #fullCheck.
		LargeInteger. #+. 
		Magnitude. #max:.
		Object. #at:.
		ProtoObject. #withArgs:executeMethod:. 
		SequenceableCollection. #do:.
		SmallInteger. #'//'. 
		SmallInteger. #*. 
		SmallInteger. #-. 
		Symbol. #flushCache.
		Number. #isZero. 
		Object. #shallowCopy. 
		OrderedCollection. #do:. 
		ProtoObject. #initialize. 
		SequenceableCollection. #first. 
		SmallInteger. #=.
		TraitBehavior. #methodDict. 
		UndefinedObject. #notNil.

		"DelayMicrosecondScheduler. #handleTimerEvent:.
		DelayMicrosecondScheduler. #primSignal:atUTCMicroseconds:. 
		Semaphore. #consumeAllSignals. 
		Semaphore. #wait. "

	}.
	
	blackList pairsDo: [ :aClass :aSelector |
		(aMethod methodClass == aClass and: [ aMethod selector == aSelector ]) 
			ifTrue: [ ^ false ] ].		
	
	^ true
]

{ #category : #'instance creation' }
MethodTracer class >> installOn: methods [

	| methodsToInstall |
	
	methodsToInstall := methods select: [ :each | self canInstall: each ].

	wrappers := methodsToInstall collect: [ :m | 
		 (MethodTracer on: m) ].
	
	wrappers do: [ :each | each install ].
	
	
]

{ #category : #'instance creation' }
MethodTracer class >> on: aCompiledMethod [

	^ self basicNew initializeOn: aCompiledMethod
]

{ #category : #'instance creation' }
MethodTracer class >> uninstall [

	wrappers do: [ :each | each uninstall ].
]

{ #category : #'instance creation' }
MethodTracer class >> wrappers [

	^ wrappers
]

{ #category : #private }
MethodTracer >> doesNotUnderstand: aMessage [
	^ method perform: aMessage selector withArguments: aMessage arguments
]

{ #category : #testing }
MethodTracer >> hasRun [
	^ hasRun
]

{ #category : #initialization }
MethodTracer >> initializeOn: aCompiledMethod [

	hasRun := false.
	method := aCompiledMethod.
	methodClass := aCompiledMethod methodClass.
	selector := aCompiledMethod selector.
]

{ #category : #actions }
MethodTracer >> install [
	methodClass methodDict
		at: selector
		put: self.
]

{ #category : #private }
MethodTracer >> mark [
	hasRun := true
]

{ #category : #accessing }
MethodTracer >> method [
	^ method
]

{ #category : #evaluation }
MethodTracer >> run: aSelector with: anArray in: aReceiver [
	self mark; uninstall.
	^ aReceiver withArgs: anArray executeMethod: method
]

{ #category : #actions }
MethodTracer >> uninstall [
	methodClass methodDict
		at: selector
		put: method.
]
