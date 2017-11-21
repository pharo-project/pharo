Class {
	#name : #RenrakuGlobalBanningTest,
	#superclass : #ReCodeBasedTestCase,
	#instVars : [
		'oldRulesSettings'
	],
	#category : #Renraku-Test
}

{ #category : #running }
RenrakuGlobalBanningTest >> setUp [
	super setUp.
	oldRulesSettings := Dictionary new.

	ReAbstractRule withAllSubclassesDo: [ :rule |
		oldRulesSettings at: rule put: rule enabled ]
]

{ #category : #running }
RenrakuGlobalBanningTest >> tearDown [
	oldRulesSettings keysAndValuesDo: [ :rule :setting |
		rule enabled: setting ].
	super tearDown
]

{ #category : #tests }
RenrakuGlobalBanningTest >> testForcedRuleOnClassWithGlobalBan [
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	ReImageRuleBaner ban: criticToSkip.
	
	testPackage packageManifest class compile:
		'forcedRules ^ #(',
		ScreamerRule name,
		')'.
		
	ReRuleManager reset.
	
	self assert: (testClass critiques anySatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuGlobalBanningTest >> testForcedRuleOnClassWithPackageBan [
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testPackage ban: criticToSkip.
	
	testPackage packageManifest class compile:
		'forcedRules ^ #(',
		ScreamerRule name,
		')'.
		
	ReRuleManager reset.
	
	self assert: (testClass critiques anySatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuGlobalBanningTest >> testForcedRuleOnMethodWithGlobalBan [
	| criticToSkip |
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	ReImageRuleBaner ban: criticToSkip.
	
	testPackage packageManifest class compile:
		'forcedRules ^ #(',
		ScreamerRule name,
		')'.
		
	ReRuleManager reset.
	
	self assert: (testMethod critiques anySatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuGlobalBanningTest >> testForcedRuleOnMethodWithPackageBan [
	| criticToSkip |
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testPackage ban: criticToSkip.
	
	testPackage packageManifest class compile:
		'forcedRules ^ #(',
		ScreamerRule name,
		')'.
		
	ReRuleManager reset.
	
	self assert: (testMethod critiques anySatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuGlobalBanningTest >> testGlobalBan [
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	ReImageRuleBaner ban: criticToSkip.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]
