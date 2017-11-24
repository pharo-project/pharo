"
##MetacelloBaselineConstructor

**MetacelloBaselineConstructor** extracts the #baseline pragma from a **BaselineOfConfiguration** and produces a **MetacelloVersion**:.

```Smalltalk
MetacelloBaselineConstructor on: BaselineOfExample
```

"
Class {
	#name : #MetacelloBaselineConstructor,
	#superclass : #MetacelloAbstractVersionConstructor,
	#category : #'Metacello-Core-Constructors'
}

{ #category : #'instance creation' }
MetacelloBaselineConstructor class >> on: aConfig [
    ^ self new
        on: aConfig;
        yourself
]

{ #category : #'instance creation' }
MetacelloBaselineConstructor class >> on: aConfig project: aProject [
    ^ self new
        on: aConfig project: aProject;
        yourself
]

{ #category : #private }
MetacelloBaselineConstructor >> calculate: aConfig project: aProject [
    | pragma versionMap versionSpec |
    self configuration: aConfig.
    pragma := self extractBaselinePragmaFor: aConfig class.
    self
        setProject:
            (aProject
                ifNil: [ 
                    [ aConfig class project ]
                        on: MessageNotUnderstood
                        do: [ :ex | ex return: nil ] ]).
    versionSpec := self project versionSpec.
    self root: versionSpec.
    self evaluatePragma: pragma.
    versionMap := Dictionary new.
    self project attributes
        do: [ :attribute | 
            | blockList |
            (blockList := self attributeMap at: attribute ifAbsent: [  ]) ~~ nil
                ifTrue: [ blockList do: [ :block | self with: versionSpec during: block ] ] ].
    versionSpec versionString: self project singletonVersionName.
    versionMap at: versionSpec versionString put: versionSpec createVersion.
    self project map: versionMap.
    self project configuration: aConfig
]

{ #category : #'pragma extraction' }
MetacelloBaselineConstructor >> extractBaselinePragmaFor: aClass [
    | pragmas |
    pragmas := Pragma allNamed: #'baseline' in: aClass.
    pragmas isEmpty
        ifTrue: [ ^ self error: 'No #baseline pragma found' ].
    ^ pragmas first
]

{ #category : #initialization }
MetacelloBaselineConstructor >> on: aConfig [
    self calculate: aConfig project: nil
]

{ #category : #initialization }
MetacelloBaselineConstructor >> on: aConfig project: aProject [
    self calculate: aConfig project: aProject
]
