"
I represent the addition of a method to a class.  I can produce the CompiledMethod, install it, and then notify the system that the method has been added.  This allows Monticello to implement atomic addition.  A loader can compile all classes and methods first and then install all methods only after they have been all compiled, and in a way that executes little code.
"
Class {
	#name : 'MethodAddition',
	#superclass : 'Object',
	#instVars : [
		'changeStamp',
		'myClass',
		'protocolName',
		'source'
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
	myClass := aClass
]

{ #category : 'operations' }
MethodAddition >> installMethod [

	('Compiling ' , (source asString copyReplaceAll: String cr with: String lf) , ' into : ' , myClass name) traceCr.

	"We are not using `myClass compiler` because the class could add itself as a compiler plugin but when we are loading it, it does not have its methods yet and the plugin cannot be used yet."
	^ myClass compilerClass new
		  environment: myClass environment;
		  class: myClass;
		  source: source asString;
		  permitUndeclared: true;
		  protocol: protocolName;
		  changeStamp: changeStamp;
		  install
]
