"
I plug nautilus with a feedback from smalllint
"
Class {
	#name : #QANautilusPlugin,
	#superclass : #AbstractNautilusPlugin,
	#instVars : [
		'morph',
		'currentEntity'
	],
	#category : 'QualityAssistant'
}

{ #category : #position }
QANautilusPlugin class >> defaultPosition [

	^ #bottom 
]

{ #category : #installing }
QANautilusPlugin class >> install [

	self isInstalled ifFalse: [
		Nautilus pluginClasses:
			(Nautilus pluginClasses copyWith:
				{ self . self defaultPosition }) ]
]

{ #category : #installing }
QANautilusPlugin class >> isInstalled [

	^ Nautilus pluginClasses anySatisfy: [ :pluginBinding | 
			pluginBinding first = self ]
]

{ #category : #display }
QANautilusPlugin class >> pluginName [
	^ 'Quality Assistant'
]

{ #category : #information }
QANautilusPlugin class >> possiblePositions [

	^ { #middle . #bottom }
]

{ #category : #installing }
QANautilusPlugin class >> uninstall [

	Nautilus pluginClasses:
		(Nautilus pluginClasses reject: [ :each | 
			each first = self ])
]

{ #category : #initialize-release }
QANautilusPlugin class >> unload [

	self uninstall 
]

{ #category : #'announcement handling' }
QANautilusPlugin >> classSelected: anAnnouncement [

	currentEntity := anAnnouncement selectedClass.
	self updateMorph  
]

{ #category : #'announcement handling' }
QANautilusPlugin >> critiqueBanned: ann [

	ann entity = self currentEntity ifTrue: [ 
		self updateMorph ]
]

{ #category : #'interaction delegation' }
QANautilusPlugin >> critiqueSelected: aCritique [
	aCritique sourceAnchor providesInterval ifTrue: [ 
		self model codeTextMorph
			setSelection: aCritique sourceAnchor interval;
			takeKeyboardFocus ]
]

{ #category : #properties }
QANautilusPlugin >> currentEntity [

	^ currentEntity
]

{ #category : #display }
QANautilusPlugin >> display [

   morph := QANautilusPluginMorph new.
	morph pluginDelegate: self.
	
	self updateMorph.
		
	^ morph
]

{ #category : #'announcement handling' }
QANautilusPlugin >> methodSelected: anAnnouncement [

	currentEntity := anAnnouncement selectedMethod.
	self updateMorph  
]

{ #category : #accessing }
QANautilusPlugin >> morph [
	^ morph
]

{ #category : #'announcement handling' }
QANautilusPlugin >> otherSelected: anAnnouncement [

	 (anAnnouncement class == NautilusPackageSelected or:
	[ anAnnouncement class == NautilusProtocolSelected ]) ifFalse: [ ^ self ].

	currentEntity := nil.
	self updateMorph  
]

{ #category : #'announcement handling' }
QANautilusPlugin >> packageSelected: anAnnouncement [

	currentEntity := anAnnouncement selectedPackage package.
	self updateMorph  

]

{ #category : #registration }
QANautilusPlugin >> registerTo: aModel [

	aModel announcer
		when: NautilusPackageSelected  send: #packageSelected:  to: self;
 		when: NautilusClassSelected    send: #classSelected:  to: self; 
		when: NautilusProtocolSelected send: #otherSelected:  to: self; 	 
		when: NautilusMethodSelected   send: #methodSelected: to: self.
		
	ReSystemAnnouncer uniqueInstance weak
		when: ReCritiqueBanned
		send: #critiqueBanned:
		to: self
	
]

{ #category : #'announcement handling' }
QANautilusPlugin >> updateMorph [
	morph ifNil: [ ^ self ].
	
	morph entity: self currentEntity
	
]
