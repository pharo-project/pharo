"
I'm an object that saves a buffer of keyevents for the morph I'm attached.
I am the one that dispatches the single and multiple shortcuts.
If the morph has a keymap that matches the keyboard event, I tell the keymap event to execute with the morph I'm attached.
"
Class {
	#name : #KMDispatcher,
	#superclass : #Object,
	#instVars : [
		'target',
		'currentEvent',
		'targets',
		'morph',
		'directKeymaps'
	],
	#category : #'Keymapping-Core-Dispatching'
}

{ #category : #'instance creation' }
KMDispatcher class >> target: aTarget [
	^ self target: #yourself morph: aTarget
]

{ #category : #'instance creation' }
KMDispatcher class >> target: aTargetSelector morph: aMorph [
	^ (self new)
		target: (KMTarget for: aTargetSelector in: aMorph);
		yourself
]

{ #category : #match }
KMDispatcher >> announcer [

	^ self target announcer
]

{ #category : #building }
KMDispatcher >> attachCategory: aCategory [
	self attachCategory: aCategory targetting: morph
]

{ #category : #building }
KMDispatcher >> attachCategory: aCategoryName onProperty: aProperty [
	self targets add: (KMCategoryBinding 
						target: [ morph perform: aProperty ] 
						morph: morph 
						category: (KMRepository default categoryForName: aCategoryName) )
]

{ #category : #building }
KMDispatcher >> attachCategory: aCategory targetting: anObject [
	
	|  category categoryTarget |
	category := aCategory asKmCategoryIn: KMRepository default.
	categoryTarget := category bindToObject: anObject andMorph: morph.
	self targets add: categoryTarget.
]

{ #category : #building }
KMDispatcher >> bindKeyCombination: aShortcut toAction: anAction [
	self directKeymaps 
		addKeymapEntry: (KMKeymap
			shortcut: aShortcut 
			action: anAction)
]

{ #category : #match }
KMDispatcher >> buffer [
	^ KMBuffer uniqueInstance buffer
]

{ #category : #match }
KMDispatcher >> completeMatch: aKeymapEntry buffer: aBuffer [
	KMLog logCompleteMatchBetween: morph and: aKeymapEntry.
	KMBuffer uniqueInstance completeMatch.
	self announcer announce: (KMCompleteMatch event: currentEvent from: self).
]

{ #category : #building }
KMDispatcher >> detachAllKeymapCategories [

	self targets removeAll
]

{ #category : #building }
KMDispatcher >> detachKeymapCategory: aCategoryName [

	self detachKeymapCategory: aCategoryName targetting: morph
]

{ #category : #building }
KMDispatcher >> detachKeymapCategory: aCategoryName targetting: anObject [
	self targets
		detect: [ :tgt | tgt target = anObject and: [ tgt category name = aCategoryName ] ]
		ifFound: [ :categoryTarget | self targets remove: categoryTarget ]
		ifNone: [ self error: 'Category ' , aCategoryName , ' is not attached to ' , morph asString ]
]

{ #category : #dispatching }
KMDispatcher >> directKeymaps [
	^directKeymaps ifNil: [ directKeymaps := KMCategory new ]
]

{ #category : #dispatching }
KMDispatcher >> dispatch: anEventBuffer [

	self keymapObservers do: [ :aTarget | 
		"nice hack to stop in the first listener"
		aTarget 
			verifyMatchWith: anEventBuffer 
			notifying: self
			thenDoing: [ ^self ] ].
	self noMatch
]

{ #category : #dispatching }
KMDispatcher >> dispatch: anEventBuffer inCategories: categories [

	(self keymapObserversForCategories: categories) do: [ :aTarget | 
		"nice hack to stop in the first listener"
		aTarget 
			verifyMatchWith: anEventBuffer 
			notifying: self
			thenDoing: [ ^self ] ].
	self noMatch
]

{ #category : #dispatching }
KMDispatcher >> dispatchKeystroke: aKeyEvent [

	| chain |
	KMLog log: aKeyEvent .

	KMBuffer uniqueInstance addEvent: aKeyEvent.

	chain := KMDispatchChain from: (KMGlobalDispatcher new dispatcher: self; yourself) andDispatcher: self.
	chain dispatch: aKeyEvent.
]

{ #category : #testing }
KMDispatcher >> includesKeymapCategory: aCategoryName [ 

	^self includesKeymapCategory: aCategoryName targetting: morph
]

{ #category : #testing }
KMDispatcher >> includesKeymapCategory: aCategoryName targetting: anObject [

	^self targets anySatisfy: [ :tgt | 
		tgt target = anObject and: [ tgt category name = aCategoryName ] ]
		
]

{ #category : #building }
KMDispatcher >> keymapForShortcut: aShortcut [

	^ self directKeymaps keymapForShortcut: aShortcut
]

{ #category : #dispatching }
KMDispatcher >> keymapObservers [
	| o |
	o := OrderedCollection with: self perInstanceTarget.
	o addAll: self targets.
	^ o
]

{ #category : #dispatching }
KMDispatcher >> keymapObserversForCategories: categories [

	^categories collect: [ :cat | KMCategoryBinding 
									target: target realTarget 
									morph: target morph 
									category: cat ].
]

{ #category : #match }
KMDispatcher >> noMatch [
]

{ #category : #match }
KMDispatcher >> partialMatch [
	| event |
	KMBuffer uniqueInstance partialMatch.
	event := KMBuffer uniqueInstance currentEvent.
	event isKeyboard not ifTrue: [ event := nil ].
	self announcer announce: (KMPartialMatch event: event from: self).
]

{ #category : #dispatching }
KMDispatcher >> perInstanceTarget [
	^KMCategoryBinding target: morph morph: morph category: self directKeymaps.
]

{ #category : #building }
KMDispatcher >> removeKeyCombination: aShortcut [
	| keymap removalTarget |

	removalTarget := self directKeymaps.
	
	keymap := self keymapForShortcut: aShortcut.
	keymap ifNil: [ 
		self targets do: [ :e | (e keymapForShortcut: aShortcut) 
			ifNotNil: [ :s | 
				removalTarget := e category.
				keymap := s ] ] ].
	keymap ifNil: [ ^ self ].
	
	removalTarget removeKeymapEntry: keymap
]

{ #category : #initialization }
KMDispatcher >> reset [
	self resetTargets.
	self resetPerInstanceTarget
]

{ #category : #initialize }
KMDispatcher >> resetPerInstanceTarget [
	directKeymaps := nil
]

{ #category : #initialize }
KMDispatcher >> resetTargets [
    targets := nil
]

{ #category : #accessing }
KMDispatcher >> target [
	^ target
]

{ #category : #accessing }
KMDispatcher >> target: aTarget [
	target := aTarget.
	morph := target morph.
	"self announcer weak
		on: MorphLostFocus send: #clearBuffer to: KMBuffer uniqueInstance."
]

{ #category : #dispatching }
KMDispatcher >> targets [
	^targets ifNil: [ targets := Set new ]
]
