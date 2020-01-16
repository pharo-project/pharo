"
I should be used as singleton which provides sets of rules for checking classes and methods.

Use:

ReRuleManager uniqueInstance classRules ""to get rules for checking classes""
ReRuleManager uniqueInstance methodRules ""to get rules for checking methods""
"
Class {
	#name : #ReRuleManager,
	#superclass : #Object,
	#instVars : [
		'rules'
	],
	#classInstVars : [
		'default',
		'managers'
	],
	#category : #'Renraku-Utility'
}

{ #category : #utilities }
ReRuleManager class >> availableRuleGroups [
	^ (self visibleRuleClasses
		collect: [ :r | r new group ]
		as: Set)
			asArray sorted
]

{ #category : #'event subscriptions' }
ReRuleManager class >> classAddedOrRemoved: aClassAddedAnnouncement [ 
	| class |
	class := aClassAddedAnnouncement classAffected.

	((class inheritsFrom: RBLintRule) or: [ 
	  class inheritsFrom: ReAbstractRule ]) 
		ifTrue: [ self reset ] 
]

{ #category : #cleanup }
ReRuleManager class >> cleanUp [

	self reset
]

{ #category : #'event subscriptions' }
ReRuleManager class >> critiqueBanned: aReCritiqueBanned [
	| baner |
	baner := aReCritiqueBanned entityBanLevel.
	
	baner class = RPackage
		ifTrue: [ self removeManagerFor: baner ]
]

{ #category : #'instance creation' }
ReRuleManager class >> default [
	^ default ifNil: [ default := self newWithRules: self defaultRules ]
]

{ #category : #helpers }
ReRuleManager class >> defaultRules [
	^ self visibleRuleClasses
			reject: [ :ruleClass |
				"lint rules suck again…"
				((ruleClass inheritsFrom: ReAbstractRule) and: [ ruleClass enabled not ]) or: [ 
				self excludedRuleNames includes: ruleClass name ] ] 
			thenCollect: #new
]

{ #category : #helpers }
ReRuleManager class >> excludedRuleNames [
	"here rules can be excluded, but it is better to implement #enabled on the rule to return false to disable it. Do not forget to call #reset on this class after changing this method!"
	^ #()
]

{ #category : #'class initialization' }
ReRuleManager class >> initialize [
	self allInstances do: [ :inst |
		self flag: 'remove in future. this is just to stop
		            the announcements from choaking old instances'.
		SystemAnnouncer uniqueInstance unsubscribe: inst ].

	self reset.
	self subscribe.
]

{ #category : #'instance creation' }
ReRuleManager class >> managerFor: aCodeEntity [ 
	^ self managers
		at: aCodeEntity package
		ifAbsentPut: [ self newManagerForPackage: aCodeEntity package ]
]

{ #category : #acccessing }
ReRuleManager class >> managers [
	^ managers ifNil: [ managers := WeakKeyDictionary new ]
]

{ #category : #'instance creation' }
ReRuleManager class >> newManagerForPackage: aRPackage [ 
	| builder manifest defaultRules rules |
	aRPackage ifNil: [ ^ self default ].
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: aRPackage.
	manifest ifNil: [ ^ self default ].
	
	defaultRules := self defaultRules.
	"remove rules banned for the package"
	rules := defaultRules reject: [ :rule |
		builder bansRule: rule for: aRPackage ].
	(rules size = defaultRules size and: [ 
	 manifest forcedRules isEmpty ])
		ifTrue: [ ^ self default ].
	"add rules forced for the packages"
	manifest forcedRules do: [ :ruleClass |
		rules addIfNotPresent:
			(self class environment at: ruleClass) new ].
	^ self newWithRules: rules
]

{ #category : #'instance creation' }
ReRuleManager class >> newWithRules: aCollection [ 
	^ self basicNew
		initializeRules: aCollection;
		yourself
]

{ #category : #'event subscriptions' }
ReRuleManager class >> removeManagerFor: anRPackage [

	self managers
		removeKey: anRPackage 
		ifAbsent: [  ]
	
]

{ #category : #initialization }
ReRuleManager class >> reset [
	<script>
	managers := nil.
	default := nil.
]

{ #category : #utilities }
ReRuleManager class >> resetWithInform [

	self reset.
	self inform: 'Cache of the Renraku rule manager has been reset'
]

{ #category : #settings }
ReRuleManager class >> ruleToggleGroupID [

	^ #ruleToggle
]

{ #category : #settings }
ReRuleManager class >> ruleToggleSettingsOn: aBuilder [ 
	<systemsettings>
	(aBuilder group: self ruleToggleGroupID)
		order: 1;
		label: 'Toggle rules to run';
		description: 'Select which rules do you want to see in the live feedback';
		parent: #qualityAssistant;
		with: [ 
			(self visibleRuleClasses sorted: [ :a :b | a name < b name ])
				select: [ :rule | rule inheritsFrom: ReAbstractRule ] "because lint rules don't have #enabled"
				thenDo: [ :rule | 
					| inst |
					inst := rule new.
					(aBuilder setting: rule enabledSettingID)
					selector: #enabled;
					target: rule;
					default: true;
					label: inst name;
					description: inst rationale ] ]
]

{ #category : #'event subscriptions' }
ReRuleManager class >> subscribe [
	<systemEventRegistration>
	self unsubscribe.
	
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded   send: #classAddedOrRemoved: to: self;
		when: ClassRemoved send: #classAddedOrRemoved: to: self.
		
	ReSystemAnnouncer uniqueInstance weak
		when: ReCritiqueBanned
		send: #critiqueBanned:
		to: self
]

{ #category : #'instance creation' }
ReRuleManager class >> uniqueInstance [
	"Not a singleton anymore"
	^ self default
]

{ #category : #'initialize-release' }
ReRuleManager class >> unload [

	self reset
]

{ #category : #'event subscriptions' }
ReRuleManager class >> unsubscribe [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	ReSystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #utilities }
ReRuleManager class >> visibleLintRuleClasses [

	^ RBLintRule withAllSubclasses select: #isVisible
]

{ #category : #utilities }
ReRuleManager class >> visibleRuleClasses [

	^ (ReAbstractRule withAllSubclasses select: #isVisible)
	"remove this in the future ->", self visibleLintRuleClasses
]

{ #category : #accessing }
ReRuleManager >> allRules [

	^ rules
]

{ #category : #'announcement handling' }
ReRuleManager >> classAdded: aClassAddedAnnouncement [ 

	(aClassAddedAnnouncement classAdded isKindOf: RBLintRule) 
		ifTrue: [ self reset ] 
]

{ #category : #accessing }
ReRuleManager >> classRules [

	^ self allRules select: [ :rule | rule class checksClass ]
]

{ #category : #initialization }
ReRuleManager >> initializeRules: aCollection [

	rules := aCollection copy
]

{ #category : #accessing }
ReRuleManager >> methodRules [

	^ self allRules select: [ :rule | rule class checksMethod ]
]

{ #category : #accessing }
ReRuleManager >> nodeRules [
	
	^ self allRules select: [ :rule | rule class checksNode ]
]

{ #category : #accessing }
ReRuleManager >> packageRules [

	^ self allRules select: [ :rule | rule class checksPackage ]
]
