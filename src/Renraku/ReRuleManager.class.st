"
I should be used as singleton which provides sets of rules for checking classes and methods.

Use:

ReRuleManager uniqueInstance classRules ""to get rules for checking classes""
ReRuleManager uniqueInstance methodRules ""to get rules for checking methods""
"
Class {
	#name : 'ReRuleManager',
	#superclass : 'Object',
	#instVars : [
		'rules'
	],
	#classVars : [
		'RulesProfile'
	],
	#classInstVars : [
		'default',
		'managers'
	],
	#category : 'Renraku-Utility',
	#package : 'Renraku',
	#tag : 'Utility'
}

{ #category : 'utilities' }
ReRuleManager class >> availableRuleGroups [
	^ (self visibleRuleClasses
		collect: [ :r | r new group ]
		as: Set)
			asArray sorted
]

{ #category : 'event subscriptions' }
ReRuleManager class >> classAddedOrRemoved: aClassAddedAnnouncement [
	| class |
	class := aClassAddedAnnouncement classAffected.

	((class inheritsFrom: RBLintRule) or: [
	  class inheritsFrom: ReAbstractRule ])
		ifTrue: [ self reset ]
]

{ #category : 'cleanup' }
ReRuleManager class >> cleanUp [

	self reset
]

{ #category : 'event subscriptions' }
ReRuleManager class >> critiqueBanned: aReCritiqueBanned [
	| baner |
	baner := aReCritiqueBanned entityBanLevel.

	baner class = Package
		ifTrue: [ self removeManagerFor: baner ]
]

{ #category : 'instance creation' }
ReRuleManager class >> default [
	^ default ifNil: [ default := self newWithRules: self defaultRules ]
]

{ #category : 'helpers' }
ReRuleManager class >> defaultRules [
	^ self visibleRuleClasses
			select: [ :ruleClass | ruleClass enabled ]
			thenCollect: [:ruleClass | ruleClass new]
]

{ #category : 'reflective operations' }
ReRuleManager class >> doesNotUnderstand: aMessage [ 
	"
	Currently, the setting model requires for each setting a getter and a setter method existing to execute. This does not work with the way rules group settings are implemented because we want to be able to add groups dynamically, thus we build the settings collecting the groups present in the image and this does not work with having pre-existing methods to execute through a setting.
	Since we cannot have those methods and it would take too much efforts currently to update the settings model, we are handling those missing getter/setter with the doesNotUnderstood:.
	
	We build the getters and setters for the settings in the method #ruleToggleSettingsOn:
	"
	(aMessage selector beginsWith: 'toggleGroupNamed')
		ifTrue: [
			| ruleName rules |
			ruleName := (aMessage selector allButFirst: 16) withoutSuffix: ':'.
			rules := self visibleRuleClasses select: [ :class |
				         class group = ruleName ].
			^ (aMessage selector endsWith: ':')
				  ifTrue: [ rules do: [ :rule | rule enabled: aMessage arguments first] ]
				  ifFalse: [ rules anySatisfy: [ :rule | rule enabled] ] ]
		ifFalse: [ super doesNotUnderstand: aMessage ]
]

{ #category : 'class initialization' }
ReRuleManager class >> initialize [
	self reset.
	self subscribe.
]

{ #category : 'instance creation' }
ReRuleManager class >> managerFor: aCodeEntity [
	^ self managers
		at: aCodeEntity package
		ifAbsentPut: [ self newManagerForPackage: aCodeEntity package ]
]

{ #category : 'accessing' }
ReRuleManager class >> managers [
	^ managers ifNil: [ managers := WeakKeyDictionary new ]
]

{ #category : 'instance creation' }
ReRuleManager class >> newManagerForPackage: aPackage [
	| builder manifest defaultRules rules |
	aPackage ifNil: [ ^ self default ].
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: aPackage.
	manifest ifNil: [ ^ self default ].

	defaultRules := self defaultRules.
	"remove rules banned for the package"
	rules := defaultRules reject: [ :rule |
		builder bansRule: rule for: aPackage ].
	(rules size = defaultRules size and: [
	 manifest forcedRules isEmpty ])
		ifTrue: [ ^ self default ].
	"add rules forced for the packages"
	manifest forcedRules do: [ :ruleClass |
		rules addIfNotPresent:
			(self class environment at: ruleClass) new ].
	^ self newWithRules: rules
]

{ #category : 'instance creation' }
ReRuleManager class >> newWithRules: aCollection [
	^ self basicNew
		initializeRules: aCollection;
		yourself
]

{ #category : 'event subscriptions' }
ReRuleManager class >> removeManagerFor: anPackage [

	self managers
		removeKey: anPackage
		ifAbsent: [  ]
]

{ #category : 'class initialization' }
ReRuleManager class >> reset [
	<script>
	managers := nil.
	default := nil
]

{ #category : 'utilities' }
ReRuleManager class >> resetWithInform [

	self reset.
	self inform: 'Cache of the Renraku rule manager has been reset'
]

{ #category : 'accessing' }
ReRuleManager class >> ruleProfileSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #rulesProfile)
		order: 1;
		label: 'Select a quality rules profile ';
		description: 'Select a profile according to your experience with Pharo. This will allow the QualityAssistant to show the rules that are appropiate to your level.';
		parent: #qualityAssistant;
		target: self;
		default: #setDefaultProfile;
		domainValues: {'Default' translated -> #setDefaultProfile .
							'Beginner' translated -> #setBeginnerProfile }
]

{ #category : 'settings' }
ReRuleManager class >> ruleToggleGroupID [

	^ #ruleToggle
]

{ #category : 'settings' }
ReRuleManager class >> ruleToggleSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: self ruleToggleGroupID)
		order: 1;
		label: 'Toggle rules to run';
		description:
			'Select which rules do you want to see in the live feedback';
		parent: #qualityAssistant;
		with: [
			(self visibleRuleClasses groupedBy: [ :class | class group ])
				keysAndValuesDo: [ :groupName :rules |
					(aBuilder setting: groupName asSymbol)
						target: self;
						selector: #toggleGroupNamed , groupName capitalized;
						default: true;
						noOrdering;
						iconName: #smallConfiguration;
						label: groupName , ' rules';
						description: 'select ', groupName , ' rules';
						with: [ self subSettingsOn: aBuilder with: rules] ] ]

]

{ #category : 'accessing' }
ReRuleManager class >> rulesProfile [

	^ RulesProfile ifNil: [ RulesProfile := #setDefaultProfile ]
]

{ #category : 'accessing' }
ReRuleManager class >> rulesProfile: aSelector [

	RulesProfile := aSelector.
	self perform: RulesProfile
]

{ #category : 'accessing' }
ReRuleManager class >> setBeginnerProfile [

	self visibleRuleClasses do: [ :rule | rule enabled: rule isBeginnerRule ].
	self reset
]

{ #category : 'accessing' }
ReRuleManager class >> setDefaultProfile [

	self visibleRuleClasses do: [ :rule | rule enabled: rule isDefaultRule ].
	self reset
]

{ #category : 'settings' }
ReRuleManager class >> subSettingsOn: aBuilder with: rules [ 

	 ^ (rules sorted: [ :a :b | a name < b name ]) do: [ :rule |
		  (aBuilder setting: rule enabledSettingID)
			  selector: #enabled;
			  target: rule;
			  default: rule enabled;
			  label: rule ruleName;
			  description: rule rationale ]
]

{ #category : 'event subscriptions' }
ReRuleManager class >> subscribe [

	<systemEventRegistration>
	self unsubscribe.

	self codeChangeAnnouncer weak
		when: ClassAdded send: #classAddedOrRemoved: to: self;
		when: ClassRemoved send: #classAddedOrRemoved: to: self.

	ReSystemAnnouncer uniqueInstance weak when: ReCritiqueBanned send: #critiqueBanned: to: self
]

{ #category : 'instance creation' }
ReRuleManager class >> uniqueInstance [
	"Not a singleton anymore"
	^ self default
]

{ #category : 'initialize-release' }
ReRuleManager class >> unload [

	self reset
]

{ #category : 'event subscriptions' }
ReRuleManager class >> unsubscribe [

	self codeChangeAnnouncer unsubscribe: self.
	ReSystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : 'utilities' }
ReRuleManager class >> visibleRuleClasses [

	^ (ReAbstractRule withAllSubclasses select: [:each | each isVisible])
	"remove this in the future ->", RBLintRule withAllSubclasses select: [:each | each isVisible]
]

{ #category : 'accessing' }
ReRuleManager >> allRules [

	^ rules
]

{ #category : 'accessing' }
ReRuleManager >> classRules [

	^ self allRules select: [ :rule | rule class checksClass ]
]

{ #category : 'initialization' }
ReRuleManager >> initializeRules: aCollection [

	rules := aCollection copy
]

{ #category : 'accessing' }
ReRuleManager >> methodRules [

	^ self allRules select: [ :rule | rule class checksMethod ]
]

{ #category : 'accessing' }
ReRuleManager >> nodeRules [

	^ self allRules select: [ :rule | rule class checksNode ]
]

{ #category : 'accessing' }
ReRuleManager >> packageRules [

	^ self allRules select: [ :rule | rule class checksPackage ]
]
