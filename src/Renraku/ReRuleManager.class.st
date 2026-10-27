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

{ #category : 'enabling' }
ReRuleManager class >> enable: aBoolean groupNamed: aRuleGroupSelector [

	(self rulesFor: aRuleGroupSelector) do: [ :rule | rule enabled: aBoolean ]
]

{ #category : 'class initialization' }
ReRuleManager class >> initialize [
	self reset.
	self subscribe.
]

{ #category : 'enabling' }
ReRuleManager class >> isGroupEnabled: aRuleGroupSelector [

	^ (self rulesFor: aRuleGroupSelector) anySatisfy: [ :rule | rule enabled ]
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
	InformativeNotification signal: 'Cache of the Renraku rule manager has been reset'
]

{ #category : 'accessing' }
ReRuleManager class >> ruleProfileSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #rulesProfile)
		order: 1;
		label: 'Select a quality rules profile ';
		description: 'Select a profile according to your experience with Pharo. This will allow the QualityAssistant to show the rules that are appropiate to your level.';
		parentName: #qualityAssistant;
		target: self;
		defaultValue: #setDefaultProfile;
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
		description: 'Select which rules do you want to see in the live feedback';
		parentName: #qualityAssistant;
		with: [
				(self visibleRuleClasses groupedBy: [ :class | class groupSelector ]) keysAndValuesDo: [ :groupNameSelector :rules |
							| groupName |
							groupName := ReAbstractRule perform: groupNameSelector.
							(aBuilder group: groupNameSelector asSymbol)
								readBlock: [ self isGroupEnabled: groupNameSelector ];
								writeBlock: [ :active | self enable: active groupNamed: groupNameSelector ];
								isActive: true;
								noOrdering;
								iconName: #smallConfiguration;
								label: groupName , ' rules';
								description: 'select ' , groupName , ' rules';
								with: [ self subSettingsOn: aBuilder with: rules ] ] ]
]

{ #category : 'utilities' }
ReRuleManager class >> rulesFor: aRuleGroupSelector [

	^ self visibleRuleClasses select: [ :class | class groupSelector = aRuleGroupSelector ]
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

	self visibleRuleClasses do: [ :ruleClass | ruleClass enabled: ruleClass isDefaultRule ].
	self reset
]

{ #category : 'settings' }
ReRuleManager class >> subSettingsOn: aBuilder with: rules [

	^ (rules sorted: [ :a :b | a name < b name ]) do: [ :rule |
			(aBuilder setting: rule enabledSettingID)
				  selector: #enabled;
				  target: rule;
				  defaultValue: rule enabledByDefault;
				  label: rule ruleName;
				  description: rule rationale ]
]

{ #category : 'event subscriptions' }
ReRuleManager class >> subscribe [

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

{ #category : 'class initialization' }
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

{ #category : 'accessing' }
ReRuleManager >> groupedRules [

	| rulesByGroup |
	(self visibleRuleClasses collect: [ :aClass |
		          aClass new ]) sorted: [ :a :b | a ruleName < b ruleName ].
	rulesByGroup := (rules groupedBy: [:each | each group]) associations collect: [ :as |
		                CBCritiquesRuleGroup
			                named: as key
			                rules: as value ].
	^ rulesByGroup
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

{ #category : 'accessing' }
ReRuleManager >> sortedGroupedRules [

	^ self groupedRules sort: [ :a :b | a name < b name ]
]

{ #category : 'accessing' }
ReRuleManager >> visibleRuleClasses [

	^ (ReAbstractRule withAllSubclasses select: [:each | each isVisible])
	"remove this in the future ->", RBLintRule withAllSubclasses select: [:each | each isVisible]
]
