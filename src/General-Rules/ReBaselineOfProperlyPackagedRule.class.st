"
Check if a baseline is properly packaged. It should be included in a package with the same name.

Example: if your baseline class is named ""BaselineOfMyKillerApp"" it should be in a package with the same name ""BaselineOfMyKillerApp""
"
Class {
	#name : 'ReBaselineOfProperlyPackagedRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'testing - interest' }
ReBaselineOfProperlyPackagedRule class >> checksClass [

	^ true
]

{ #category : 'manifest' }
ReBaselineOfProperlyPackagedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReBaselineOfProperlyPackagedRule'
]

{ #category : 'running' }
ReBaselineOfProperlyPackagedRule >> basicCheck: aClass [

	^(aClass name beginsWith: 'BaselineOf') 
		and: [ aClass package name ~= aClass name ]
]

{ #category : 'accessing' }
ReBaselineOfProperlyPackagedRule >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReBaselineOfProperlyPackagedRule >> name [
	^ 'Baseline class not properly packaged in a package with the same name'
]

{ #category : 'accessing' }
ReBaselineOfProperlyPackagedRule >> severity [
	^ #information
]
