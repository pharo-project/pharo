Class {
	#name : #PBMethodCompiler,
	#superclass : #Object,
	#instVars : [
		'builder',
		'packages'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #selection }
PBMethodCompiler >> buildPackagesList [
	| classes |
	classes := builder behaviorDefinitions.
	^((Smalltalk at: #MCWorkingCopy ifAbsent: [ ^#() ] )
		allManagers
			select: [ :m | classes anySatisfy: [ :c | m packageInfo includesClassNamed: c name ] ]
			thenCollect: [ :m | m packageInfo packageName asUppercase ]) asSet asOrderedCollection.
]

{ #category : #accessing }
PBMethodCompiler >> builder: aBuilder [
	builder := aBuilder
]

{ #category : #'method-compiling' }
PBMethodCompiler >> createNewMethod: aMethodDefinition [

"	| ownerClass |
""	ownerClass := builder behaviorFrom: aMethodDefinition methodClass theNonMetaClass."
"	aMethodDefinition methodClass isMeta ifTrue: [ ownerClass := ownerClass classSide ].
"
	| method timeToCompile |
	timeToCompile := [ method := builder objectSpace crossCompiler
		compile: aMethodDefinition sourceCode
		forClass: aMethodDefinition methodClass
		inEnvironment: builder] timeToRun.
	builder statistics addCompilationTime: timeToCompile.
	^ method
]

{ #category : #'method-compiling' }
PBMethodCompiler >> methodsForBehavior: aBehavior [
	"Get all the methods we consider we want from the current environment and return a copy, binded to the new class"
	^ aBehavior methods 
		reject: [ :m | (self shouldMethodBeRemoved: m) or: [m selector = #trace:on:] ]
		thenCollect: [ :m | self createNewMethod: m ].
]

{ #category : #'method-compiling' }
PBMethodCompiler >> methodsFromDefinition: aBehaviorDefinition [
	"Get all the methods we consider we want from the current environment and return a copy, binded to the new class"
	^ aBehaviorDefinition localMethods 
		collect: [ :m | self createNewMethod: m ].
]

{ #category : #selection }
PBMethodCompiler >> packages [
	^packages ifNil: [packages := self buildPackagesList]
]

{ #category : #selection }
PBMethodCompiler >> shouldMethodBeRemoved: aMethod [	
	| category |
	
	category := aMethod category ifNil: [ ^false ].
	
	(category asUppercase beginsWith: '*ImageWriter' asUppercase) ifTrue: [ ^false ].
		
	Smalltalk at: #MCWorkingCopy ifAbsent: [ ^false ].
	
	"Remove bad extensions"
	(category beginsWith: '*') ifTrue: [ | copy |
		copy := category allButFirst.
		[
			(self packages anySatisfy: [ :cat | copy asUppercase beginsWith: cat asUppercase ])
				ifTrue: [ ^ false ].
			copy := '-' join: (copy subStrings: '-') allButLast.
		] doWhileTrue: [ copy notEmpty ].
		^true
	].

	^ false
]
