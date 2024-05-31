"
A rule that checks whether a method is an extension and whether it overrides an existing method. Such a method is usually a bug or wrongly classified.
"
Class {
	#name : 'ReOverridingExtentsionMethod',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReOverridingExtentsionMethod class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReOverridingExtentsionMethod class >> ruleName [
	^ 'An extension method overrides another method'
]

{ #category : 'running' }
ReOverridingExtentsionMethod >> basicCheck: aMethod [
  ^ aMethod isExtension and: [
    | superMethod |
    superMethod := aMethod origin superclass ifNotNil: [ :class |
      class lookupSelector: aMethod selector ].
    superMethod isNotNil and: [
      "Ignore overrides of extension methods in special classes.
      These may be name clashes or intended overrides of framework
      methods."
      (self specialClasses includes: superMethod methodClass) not and: [
          "We allow extensions from the same package to override, so that
          hierarchies can be extended with overriding functionality."
          superMethod package ~= aMethod package ] ] ]
]

{ #category : 'private' }
ReOverridingExtentsionMethod >> specialClasses [
  ^ {ProtoObject. Object. Behavior. Class. ClassDescription}
]
