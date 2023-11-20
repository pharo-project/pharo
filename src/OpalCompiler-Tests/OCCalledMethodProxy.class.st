Class {
	#name : 'OCCalledMethodProxy',
	#superclass : 'Object',
	#instVars : [
		'originalMethod',
		'called'
	],
	#category : 'OpalCompiler-Tests-AST',
	#package : 'OpalCompiler-Tests',
	#tag : 'AST'
}

{ #category : 'instance creation' }
OCCalledMethodProxy class >> on: aMethod [

	^ self new
		  originalMethod: aMethod;
		  yourself
]

{ #category : 'accessing - pragmas & properties' }
OCCalledMethodProxy >> cachePragmas [
	
	^ originalMethod cachePragmas
]

{ #category : 'accessing' }
OCCalledMethodProxy >> called [

	^ called
]

{ #category : 'initialization' }
OCCalledMethodProxy >> initialize [

	super initialize.
	called := false
]

{ #category : 'accessing' }
OCCalledMethodProxy >> methodClass: aClass [ 
	
	originalMethod methodClass: aClass
]

{ #category : 'accessing' }
OCCalledMethodProxy >> originalMethod [

	^ originalMethod
]

{ #category : 'accessing' }
OCCalledMethodProxy >> originalMethod: anObject [

	originalMethod := anObject
]

{ #category : 'accessing' }
OCCalledMethodProxy >> propertyAt: aSymbol ifPresent: aBlock [
	"Do nothing there"

	
]

{ #category : 'evaluation' }
OCCalledMethodProxy >> run: aSelector with: arguments in: aReceiver [

	called := true.
	^ originalMethod valueWithReceiver: aReceiver arguments: arguments
]

{ #category : 'accessing' }
OCCalledMethodProxy >> selector: aString [ 
	
	originalMethod selector: aString
]

{ #category : 'testing' }
OCCalledMethodProxy >> wasCalled [
	
	^ called
]
