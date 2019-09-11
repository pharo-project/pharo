"
The rule checks if the class rule in question specifies which entities it can check
"
Class {
	#name : #RBDefineEntityComplianceCheckRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBDefineEntityComplianceCheckRule class >> checksClass [
	^ true
]

{ #category : #running }
RBDefineEntityComplianceCheckRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: RBLintRule) and: [ aClass isVisible and: [ self complianceMethods noneSatisfy: [ :method | aClass perform: method ] ] ]
]

{ #category : #properties }
RBDefineEntityComplianceCheckRule >> complianceMethods [

	^ #(checksMethod checksClass checksPackage)
]

{ #category : #accessing }
RBDefineEntityComplianceCheckRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBDefineEntityComplianceCheckRule >> name [
	^ 'Rule does not define entity compliance'
]

{ #category : #accessing }
RBDefineEntityComplianceCheckRule >> rationale [

	^ 'You should override on the CLASS SIDE at least one of #',
	  (self complianceMethods joinUsing: ', #'),
	  ' methods to return true and so indicate which entities can be checked by your rule.'
]
