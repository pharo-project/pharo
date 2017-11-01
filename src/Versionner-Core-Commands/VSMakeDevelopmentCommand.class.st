"
I'm a command to declare versions as development.
"
Class {
	#name : #VSMakeDevelopmentCommand,
	#superclass : #VSMakeVersionCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #testing }
VSMakeDevelopmentCommand >> canBeApplied [
	^ target isDevelopment not and: [ target isBaseline ]
]

{ #category : #accessing }
VSMakeDevelopmentCommand >> symbolicVersion [ 
	^ #development
]

{ #category : #accessing }
VSMakeDevelopmentCommand >> title [
	^ 'make development version'
]
