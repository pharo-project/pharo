"
I am a regex refactoring renaming protocol names.
Here is a typical examples.

```
| newClassPrefix env model |
env := RBBrowserEnvironment new 
			forClasses: (Smalltalk allClasses 
									select: [ :each | each protocolNames includes: #'initialize-release' ]).
			
model := (RBNamespace onEnvironment: env) name: 'MyModel'; yourself.

RBProtocolRegexTransformation new
	model: model;
	replace: 'initialize-release' with: 'initialization';
  execute.
```
"
Class {
	#name : 'RBProtocolRegexTransformation',
	#superclass : 'RBRegexRefactoring',
	#category : 'Refactoring-Core-Transformation-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Transformation-Unused'
}

{ #category : 'transforming' }
RBProtocolRegexTransformation >> privateTransform [

	| replacement |
	self model allClassesDo: [ :class |
		class selectors do: [ :selector |
			(class realClass protocolNameOfSelector: selector) asString ifNotNil: [ :original |
				replacement := self execute: original.
				replacement = original ifFalse: [ self generateChangesFor:
																(RBAddMethodTransformation
																	sourceCode: (class sourceCodeFor: selector)
																	in: class
																	withProtocol: replacement)] ] ] ]
]
