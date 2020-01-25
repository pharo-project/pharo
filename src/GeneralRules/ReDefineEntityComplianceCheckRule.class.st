"
The rule checks if the class rule in question specifies which entities it can check
"
Class {
	#name : #ReDefineEntityComplianceCheckRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReDefineEntityComplianceCheckRule class >> checksClass [
	^ true
]

{ #category : #running }
ReDefineEntityComplianceCheckRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: RBLintRule) and: [ aClass isVisible and: [ self complianceMethods noneSatisfy: [ :method | aClass perform: method ] ] ]
]

{ #category : #properties }
ReDefineEntityComplianceCheckRule >> complianceMethods [

	^ #(checksMethod checksClass checksPackage)
]

{ #category : #accessing }
ReDefineEntityComplianceCheckRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
ReDefineEntityComplianceCheckRule >> name [
	^ 'Rule does not define entity compliance'
]

{ #category : #accessing }
ReDefineEntityComplianceCheckRule >> rationale [

	^ 'You should override on the CLASS SIDE at least one of #',
	  (self complianceMethods joinUsing: ', #'),
	  ' methods to return true and so indicate which entities can be checked by your rule.'
]
