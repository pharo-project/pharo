"
##MetacelloBaselineConstructor

**MetacelloBaselineConstructor** extracts the #baseline pragma from a **BaselineOfConfiguration** and produces a **MetacelloVersion**:.

```Smalltalk
MetacelloBaselineConstructor on: BaselineOfExample
```

"
Class {
	#name : 'MetacelloBaselineConstructor',
	#superclass : 'MetacelloAbstractVersionConstructor',
	#category : 'Metacello-Core-Constructors',
	#package : 'Metacello-Core',
	#tag : 'Constructors'
}

{ #category : 'private' }
MetacelloBaselineConstructor >> calculate: aConfig project: aProject [

	| pragma versionMap versionSpec |
	self configuration: aConfig.
	pragma := self extractBaselinePragmaFor: aConfig class.
	self setProject: (aProject ifNil: [
			 [ aConfig class project ]
				 on: MessageNotUnderstood
				 do: [ :ex | ex return: nil ] ]).
	versionSpec := self project versionSpec.
	self root: versionSpec.
	self evaluatePragma: pragma.
	versionMap := Dictionary new.
	self project attributes do: [ :attribute |
		(self attributeMap at: attribute ifAbsent: [  ]) ifNotNil: [ :blockList | blockList do: [ :block | self with: versionSpec during: block ] ] ].
	versionSpec versionString: self project singletonVersionName.
	versionMap at: versionSpec versionString put: versionSpec createVersion.
	self project map: versionMap.
	self project configuration: aConfig
]

{ #category : 'pragma extraction' }
MetacelloBaselineConstructor >> extractBaselinePragmaFor: aClass [

	^ (Pragma allNamed: #baseline in: aClass)
		ifEmpty: [ self error: 'No #baseline pragma found' ]
		ifNotEmpty: [ :pragmas | pragmas first ]
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> projectClass [
    ^ MetacelloMCBaselineProject
]

{ #category : 'api' }
MetacelloBaselineConstructor >> version: anObject [

	self error: 'version: not allowed in a baseline project spec'
]
