"
This command allows to accept a method in a way that the unsaved editor buffer will be executed when running tests, but not in normal execution

Missing

- tool that shows all methods that are accepted for test (with a accept all button)
- a visual indicator in the footer of the browser
"
Class {
	#name : #AcceptForTestCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'selectedClass',
		'code',
		'selectedMethod'
	],
	#category : #'Reflectivity-Tools'
}

{ #category : #accessing }
AcceptForTestCommand class >> defaultMenuIconName [
	^ #smallJustified
]

{ #category : #accessing }
AcceptForTestCommand class >> defaultMenuItemName [
	^ 'Accept for Test'
]

{ #category : #execution }
AcceptForTestCommand >> execute [

	| newMethod metaLink |
	newMethod := selectedClass compiler
		source: code;
		options: #(+ optionParseErrors);
		compile.
	"the link executes the method we just created and returns"
	metaLink := MetaLink new
		metaObject: [ :aContext :args | 
			CurrentExecutionEnvironment value isTest
				ifTrue: [ aContext return: (newMethod valueWithReceiver: aContext receiver arguments: args) ] ];
		selector: #value:value:;
		arguments: #(context arguments).
	selectedMethod ast link: metaLink.
]

{ #category : #execution }
AcceptForTestCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	selectedMethod := aToolContext selectedMethod.
	selectedClass := selectedMethod methodClass.
	code := aToolContext tool textModel getString.
]
