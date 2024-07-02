Class {
	#name : 'UserDefinedBehavior',
	#superclass : 'DebugPointMetaBehavior',
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'accessing' }
UserDefinedBehavior >> context [
	^self getArgument: #context
]

{ #category : 'accessing' }
UserDefinedBehavior >> evaluate: expression withContext: aContext [

	^ Smalltalk compiler
		  source: expression;
		  context: aContext;
		  receiver: aContext receiver;
		  bindings: { (#thisContext -> aContext) };
		  evaluate
]
