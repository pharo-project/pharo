"
I check that, when deprecations react as errors (as the test command's --deprecations-as-errors sets them), a deprecation reaching the test runner is an error of the test that rewrites nothing, and that otherwise it goes on to its default action.
"
Class {
	#name : 'DeprecationAsErrorTest',
	#superclass : 'TestCase',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'helpers' }
DeprecationAsErrorTest >> runExampleWithReaction: aReaction rewriting: aRewriting [
	"Run the example test with these deprecation strategies, restoring the ones of the image afterwards. When the deprecation goes on past the example's runner, it reaches this test, which resumes it: it must not be an error of this test."

	| oldReaction oldRewriting |
	oldReaction := Deprecation reaction.
	oldRewriting := Deprecation rewriting.
	Deprecation
		reaction: aReaction;
		rewriting: aRewriting.
	^ [
		  [ (DeprecationAsErrorExampleTest selector: #testSendsDeprecatedMessage) run ]
			  on: Deprecation
			  do: [ :deprecation | deprecation resume ] ] ensure: [
			  Deprecation
				  reaction: oldReaction;
				  rewriting: oldRewriting ]
]

{ #category : 'tests' }
DeprecationAsErrorTest >> testDeprecationAsErrorRewritesNothing [
	"Even if the rewriting strategy would rewrite, an error stops the test before the default action."

	self runExampleWithReaction: ErrorDeprecationReaction rewriting: AlwaysRewriteDeprecation.
	self assert: (DeprecationAsErrorExampleTest >> #testSendsDeprecatedMessage sendsSelector: #deprecatedMessage)
]

{ #category : 'tests' }
DeprecationAsErrorTest >> testDeprecationIsAnErrorOfTheTest [

	| result |
	result := self runExampleWithReaction: ErrorDeprecationReaction rewriting: NeverRewriteDeprecation.
	self assert: result errors size equals: 1.
	self assert: result passed isEmpty
]

{ #category : 'tests' }
DeprecationAsErrorTest >> testOtherReactionsLetTheTestPass [

	| result |
	result := self runExampleWithReaction: LogDeprecationReaction rewriting: NeverRewriteDeprecation.
	self assert: result passed size equals: 1.
	self assert: result errors isEmpty
]
