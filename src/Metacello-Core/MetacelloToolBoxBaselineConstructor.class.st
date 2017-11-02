Class {
	#name : #MetacelloToolBoxBaselineConstructor,
	#superclass : #MetacelloToolBoxConstructor,
	#category : 'Metacello-Core-Constructors'
}

{ #category : #private }
MetacelloToolBoxBaselineConstructor >> evaluateBaselineMethodSection: methodSection [
  | versionSpec |
  versionSpec := self project versionSpec.
  methodSection versionSpec: versionSpec.
  currentSection := methodSection.
  self with: versionSpec during: methodSection block.
  methodSection methodSections
    do: [ :ms | self evaluateBaselineMethodSection: ms ]
]

{ #category : #'pragma extraction' }
MetacelloToolBoxBaselineConstructor >> extractBaselinePragmaFor: aClass [
    | pragmas |
    pragmas := Pragma allNamed: #'baseline' in: aClass.
    pragmas isEmpty
        ifTrue: [ ^ self error: 'No #baseline pragma found' ].
    ^ pragmas first
]

{ #category : #extraction }
MetacelloToolBoxBaselineConstructor >> extractMethodSectionsFor: aBaselineClass [
  self shouldNotImplement
]

{ #category : #extraction }
MetacelloToolBoxBaselineConstructor >> extractMethodSectionsForClass: aBaselineClass [
  | pragma |
  pragma := self extractBaselinePragmaFor: aBaselineClass.
  self evaluatePragma: pragma.
  self methodSections
    do: [ :methodSection | self evaluateBaselineMethodSection: methodSection ]
]
