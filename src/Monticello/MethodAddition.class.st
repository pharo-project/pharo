"
I represent the addition of a method to a class. I will probably be removed soon.
"
Class {
	#name : 'MethodAddition',
	#superclass : 'Object',
	#instVars : [
		'changeStamp',
		'myClass',
		'protocolName',
		'source',
		'compiler'
	],
	#category : 'Monticello-Loading',
	#package : 'Monticello',
	#tag : 'Loading'
}

{ #category : 'compilation' }
MethodAddition >> compile: aString classified: aProtocolName withStamp: aString2 inClass: aClass [

	source := aString.
	protocolName := aProtocolName.
	changeStamp := aString2.
	myClass := aClass.
	compiler := myClass compiler
]

{ #category : 'operations' }
MethodAddition >> installMethod [

	^ compiler
		  source: source asString;
		  permitUndeclared: true;
		  protocol: protocolName;
		  changeStamp: changeStamp;
		  install
]
