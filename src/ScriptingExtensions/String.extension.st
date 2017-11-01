Extension { #name : #String }

{ #category : #'*ScriptingExtensions' }
String >> asClass [
	"returns a global class with my name"
	^ self asClassInEnvironment: Smalltalk globals
]

{ #category : #'*ScriptingExtensions' }
String >> asClassIfAbsent: absentBlock [
	"returns a global class with my name"
	^ self asClassInEnvironment: Smalltalk globals ifAbsent: absentBlock
]

{ #category : #'*ScriptingExtensions' }
String >> asClassIfPresent: presentBlock [
	"returns a global class with my name"
	^ presentBlock cull: (self asClassIfAbsent: [ ^ nil ])
]

{ #category : #'*ScriptingExtensions' }
String >> asClassInEnvironment: anEnvironment [
	"I return class named after myself from anEnvironment"
	^ self 
		asClassInEnvironment: anEnvironment 
		ifAbsent: [ NotFound signalFor: self in: anEnvironment  ]
]

{ #category : #'*ScriptingExtensions' }
String >> asClassInEnvironment: anEnvironment ifAbsent: absentBlock [
	^  anEnvironment 
			at: self asSymbol 
			ifPresent: [ :aClass| 
				self 
					assert: aClass isBehavior 
					description: [ self printString, ' is not a Class but an instance of a ', aClass class name ].
				aClass ]
			ifAbsent: absentBlock
	
]
