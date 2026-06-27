"
I'm a package rule used during testing.
"
Class {
	#name : 'RePackageRuleForTest',
	#superclass : 'ReAbstractRule',
	#category : 'Renraku-Tests-Utilities',
	#package : 'Renraku-Tests',
	#tag : 'Utilities'
}

{ #category : 'testing - interest' }
RePackageRuleForTest class >> checksPackage [

	^ true
]
