"
The rule checks if the class rule in question specifies which entities it can check
"
Class {
	#name : 'ReDefineEntityComplianceCheckRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'testing' }
ReDefineEntityComplianceCheckRule class >> checksClass [
	^ true
]

{ #category : 'properties' }
ReDefineEntityComplianceCheckRule class >> complianceMethods [

	^ #(checksMethod checksClass checksPackage)
]

{ #category : 'accessing' }
ReDefineEntityComplianceCheckRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReDefineEntityComplianceCheckRule class >> rationale [

	^ 'You should override on the CLASS SIDE at least one of #' , (self complianceMethods joinUsing: ', #')
	  , ' methods to return true and so indicate which entities can be checked by your rule.'
]

{ #category : 'accessing' }
ReDefineEntityComplianceCheckRule class >> ruleName [
	^ 'Rule does not define entity compliance'
]

{ #category : 'running' }
ReDefineEntityComplianceCheckRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: RBLintRule) and: [ aClass isVisible and: [ self complianceMethods noneSatisfy: [ :method | aClass perform: method ] ] ]
]

{ #category : 'properties' }
ReDefineEntityComplianceCheckRule >> complianceMethods [
	^ self class complianceMethods
]
