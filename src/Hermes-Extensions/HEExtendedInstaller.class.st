"
The HEInstaller is really basic. 
It is designed that way to keep simple the bootstraped image. 
A bigger or complex bootstraped image requires more bootstrap time.

So, I am loaded later (with Hermes itself) and I extend the behavior in HEInstaller 

I add: 
 	- 	Having a duplication Strategy
	-	Having a configuration to fail on new undeclared variables.
"
Class {
	#name : #HEExtendedInstaller,
	#superclass : #HEInstaller,
	#instVars : [
		'failOnUndeclared',
		'duplicationMode'
	],
	#category : #'Hermes-Extensions-ClassInstaller'
}

{ #category : #accessing }
HEExtendedInstaller >> duplicationMode: anObject [
	duplicationMode := anObject
]

{ #category : #'validating existance' }
HEExtendedInstaller >> existingClass: aHEClass [
	^ duplicationMode existingClass: aHEClass on: self.
]

{ #category : #'validating existance' }
HEExtendedInstaller >> existingTrait: aHETrait [
	^ duplicationMode existingTrait: aHETrait on: self.
]

{ #category : #accessing }
HEExtendedInstaller >> failOnUndeclared: anObject [
	failOnUndeclared := anObject
]

{ #category : #initialization }
HEExtendedInstaller >> forOptions: aCommandLine [
	failOnUndeclared := (aCommandLine hasOption: 'no-fail-on-undeclared') not.

	duplicationMode := HEDuplicationModeStrategy forOption: (aCommandLine optionAt: 'on-duplication' ifAbsent: [ 'fail' ]).
]

{ #category : #initialization }
HEExtendedInstaller >> initialize [
	super initialize.
	failOnUndeclared := true.
	duplicationMode := HEFailOnDuplication new.
]

{ #category : #'reporting undeclared' }
HEExtendedInstaller >> reportNewUndeclareds: newUndeclareds [
	| message |
	newUndeclareds
		ifNotEmpty: [ message := 'New Undeclared created... check dependencies: ', newUndeclareds keys printString.
			failOnUndeclared
				ifTrue: [ self error: message ]
				ifFalse: [ SystemNotification signal: message ] ]
]

{ #category : #'validating existance' }
HEExtendedInstaller >> shouldBuildMethod: aHEMethod in: aClass [
	^ duplicationMode shouldBuildMethod: aHEMethod in: aClass installer: self.
]
