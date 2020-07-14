"
The testing framework nil out all the instance variables but it does not reinvoke the initialize method. 
It means that you can get a test breaking and when you rerun your test you get the environment of the test broken full of nil while you are thinking that the initialize should have been executed.

Initialize should only use variables defined in #instanceVariablesToKeep.
"
Class {
	#name : #ReTestCaseShouldNotUseInitializeRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #'testing-interest' }
ReTestCaseShouldNotUseInitializeRule class >> checksClass [

	^ true
]

{ #category : #accessing }
ReTestCaseShouldNotUseInitializeRule >> basicCheck: aClass [

	| alteredVariables expectedVariables |
	
	aClass isTestCase 
		ifFalse: [ ^ false ].
		
	(aClass selectors includes: #initialize)
		ifFalse: [ ^ false ].
	
	alteredVariables := (aClass >> #initialize) ast allChildren 
					select: [ :e | e isAssignment and: [ e variable isInstanceVariable ] ]
					thenCollect: [ :e | e variable ].
	
	expectedVariables := aClass new instanceVariablesToKeep.
					
	^ alteredVariables anySatisfy: [ :e | (expectedVariables includes: e name) not ].
		

]

{ #category : #accessing }
ReTestCaseShouldNotUseInitializeRule >> name [
	
	^ 'Tests should not use initialize'
]
