"
Adds a AST menu ""accept for test"". Code, even syntactically wrong, can be accepted for test.
When running tests, this code is executed, normal executin is untouched.

This is just a stand-in menu for testing, we need to move it somewhere else
(maybe close to the Accept entry).
"
Class {
	#name : #SugsAcceptForTest,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsAcceptForTest >> execute [
	| newMethod metaLink |
	newMethod := context selectedClass compiler
		source: context code;
		options: #(+ optionParseErrors);
		compile.
	"the link executes the method we just created and returns"
	metaLink := MetaLink new
		metaObject: [ :aContext :args | 
			CurrentExecutionEnvironment value isTest
				ifTrue: [ aContext return: (newMethod valueWithReceiver: aContext receiver arguments: args) ] ];
		selector: #value:value:;
		arguments: #(context arguments).
	context selectedMethod ast link: metaLink
]

{ #category : #display }
SugsAcceptForTest >> label [
	^'Accept for test' translated
]
