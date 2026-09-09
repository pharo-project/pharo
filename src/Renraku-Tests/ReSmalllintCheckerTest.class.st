Class {
	#name : 'ReSmalllintCheckerTest',
	#superclass : 'ReCodeBasedTestCase',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'tests' }
ReSmalllintCheckerTest >> testRun [
	| checker |

	checker := ReSmalllintChecker new
		rule: { ScreamerRule new };
		environment: testPackage asEnvironment;
		yourself.

	checker run.

	self assert: (checker criticsOf: ScreamerRule new) notEmpty
]

{ #category : 'tests' }
ReSmalllintCheckerTest >> testSelectPackageRule [

	| checker ruleClass packageRule |
	[
		ruleClass := self class classInstaller make: [ :aBuilder |
				             aBuilder
					             superclass: ReAbstractRule;
					             name: 'RePackageRuleForTest';
					             package: #'Generated-Rules-Tests-Generated' ].
		ruleClass class compile: 'checksPackage

	^ true'.

		packageRule := ruleClass new.
		checker := ReSmalllintChecker new
			           rule: { packageRule };
			           environment: testPackage asEnvironment;
			           reParseRule.

		self assert: (checker packageRules includes: packageRule) ] ensure: [ self packageOrganizer removePackage: #'Generated-Rules-Tests-Generated' ]
]
